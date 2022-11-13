/*
 * Copyright 2022 Polyvariant
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.polyvariant.classfile.examples

import language.dynamics

import org.polyvariant.classfile.ClassFile
import org.polyvariant.classfile.ConstantPool
import org.polyvariant.classfile.ClassAccessFlag
import org.polyvariant.classfile.Constant
import org.polyvariant.classfile.ConstantIndex
import org.polyvariant.classfile.MethodInfo
import org.polyvariant.classfile.MethodAccessFlag
import org.polyvariant.classfile.AttributeInfo
import scodec.bits._
import org.polyvariant.classfile.codecs.ClassFileCodecs
import org.polyvariant.classfile.Instruction
import cats.Functor
import cats.implicits._
import cats.data.State
import cats.data.Chain
import java.nio.file.Paths
import java.io.DataInput
import java.io.DataInputStream
import java.io.FileInputStream
import scala.reflect.ClassTag
import scala.quoted.Expr
import scala.quoted.Quotes
import org.polyvariant.classfile.Constant.Utf8Info
import java.nio.file.Files

object BytecodeCodegen extends App {

  trait PoolOps[F[_]] {
    def add[C <: Constant](c: C): F[ConstantIndex[C]]

    def addUtf8String(s: String): F[ConstantIndex[Constant.Utf8Info]] = add(
      Constant.Utf8Info(s)
    )

    def addTypeDescriptor(t: TypeDescriptor): F[ConstantIndex[Utf8Info]] = addUtf8String(t.render)

    def addClassInfo(c: ConstantIndex[Constant.Utf8Info]): F[ConstantIndex[Constant.ClassInfo]] =
      add(Constant.ClassInfo(c))

  }

  object PoolOps {

    type PoolOp[A] = State[ConstantPool, A]

    val stateInstance: PoolOps[PoolOp] =
      new PoolOps[PoolOp] {

        def add[C <: Constant](
          c: C
        ): PoolOp[ConstantIndex[C]] = State.get[ConstantPool].flatMap { old =>
          old.indexOf(c) match {
            case None =>
              val nextIndex = ConstantIndex[C](old.declaredSize + c.size)
              State.set(old.append(c)).as(nextIndex)

            case Some(i) => State.pure(i)
          }

        }

      }

  }

  def withPool[A](
    f: PoolOps[PoolOps.PoolOp] => PoolOps.PoolOp[ConstantPool => A]
  ): A = {
    val (s, a) =
      f(PoolOps.stateInstance)
        .run(ConstantPool(Nil))
        .value

    a(s)
  }

  case class TypeDescriptor(sym: String) extends Dynamic {
    def selectDynamic(name: String): TypeDescriptor = copy(s"$sym/$name")
    def render: String = sym
    def obj: TypeDescriptor = copy(s"L$sym;")
    def array: TypeDescriptor = copy(s"[$sym")
    def method(returns: TypeDescriptor): TypeDescriptor = copy(s"($sym)${returns.render}")
  }

  // Type descriptor DSL
  object T extends Dynamic {
    def string(s: String): TypeDescriptor = new TypeDescriptor(s)
    def selectDynamic(name: String): TypeDescriptor = new TypeDescriptor(name)

  }

  val UnitDescriptor: TypeDescriptor = T.V
  val StringDescriptor: TypeDescriptor = T.java.lang.String.obj

  def mkSystemOut(pool: PoolOps[PoolOps.PoolOp]) =
    for {
      system <- pool
        .addTypeDescriptor(T.java.lang.System)
        .flatMap(pool.addClassInfo)
      out <- pool.addTypeDescriptor(T.out)
      printStream <- pool.addTypeDescriptor(T.java.io.PrintStream.obj)
      nat <- pool.add(Constant.NameAndTypeInfo(out, printStream))
      fieldRefInfo <- pool.add(Constant.FieldRefInfo(system, nat))
    } yield fieldRefInfo

  def mkPrintln(pool: PoolOps[PoolOps.PoolOp]) =
    for {
      printStream <- pool
        .addTypeDescriptor(T.java.io.PrintStream)
        .flatMap(pool.addClassInfo)
      println <- pool.addTypeDescriptor(T.println)
      descriptor <- pool.addTypeDescriptor(StringDescriptor.method(UnitDescriptor))
      nat <- pool.add(Constant.NameAndTypeInfo(println, descriptor))
      methodRefInfo <- pool.add(Constant.MethodRefInfo(printStream, nat))
    } yield methodRefInfo

  def mkPrintlnCall(
    pool: PoolOps[PoolOps.PoolOp],
    text: String,
  ): PoolOps.PoolOp[(Vector[Instruction], Int)] =
    for {
      stringIndex <- pool
        .addTypeDescriptor(T.string(text))
        .flatMap(s => pool.add(Constant.StringInfo(s)))
      systemOutIndex <- mkSystemOut(pool)
      printlnIndex <- mkPrintln(pool)
    } yield {
      import Instruction._

      (
        Vector(
          getstatic(systemOutIndex),
          stringIndex.toNarrowEither.fold(ldc_w(_), ldc(_)),
          invokevirtual(printlnIndex),
        ),
        // getstatic + ldc
        2,
      )
    }

  def mkMainMethod(pool: PoolOps[PoolOps.PoolOp]): PoolOps.PoolOp[ConstantPool => MethodInfo] = {
    val mkCode =
      for {
        result <- mkPrintlnCall(pool, "Hello, world!")
      } yield {
        val (printOp, printMaxStack) = result
        import Instruction._

        AttributeModel.Code(
          maxStack = printMaxStack,
          maxLocals = 1,
          code = printOp ++ Vector(_return),
          exceptionTable = Vector(),
          attributes = List(),
        )
      }

    for {
      mainNameIndex <- pool.addUtf8String("main")
      voidDescriptorIndex <- pool.addTypeDescriptor(
        StringDescriptor.array.method(UnitDescriptor)
      )
      _ <- pool.addUtf8String("Code")
      code <- mkCode
    } yield (cp: ConstantPool) =>
      MethodInfo(
        accessFlags = Set(MethodAccessFlag.Public, MethodAccessFlag.Static),
        nameIndex = mainNameIndex,
        descriptorIndex = voidDescriptorIndex,
        attributes = AttributeCodecs.encodeAttrs(
          List(code),
          cp,
        ),
      )
  }

  val cf = withPool { pool =>
    for {
      thisClass <- pool.addTypeDescriptor(T.Foo).flatMap(pool.addClassInfo)
      superClass <- pool.addTypeDescriptor(T.java.lang.Object).flatMap(pool.addClassInfo)
      mainMethod <- mkMainMethod(pool)
      _ <- pool.addUtf8String("SYNTHETIC.java")
      _ <- pool.addUtf8String("SourceFile")
    } yield cp =>
      ClassFile(
        minorVersion = 0,
        // java 1.4
        majorVersion = 48,
        constants = cp,
        accessFlags = Set(ClassAccessFlag.Public, ClassAccessFlag.Super),
        thisClass = thisClass,
        superClass = superClass,
        interfaces = Nil,
        fields = Nil,
        methods = List(mainMethod(cp)),
        attributes = AttributeCodecs.encodeAttrs(
          List(AttributeModel.SourceFile("SYNTHETIC.java")),
          cp,
        ),
      )
  }

  pprint.pprintln(cf)

  val bytes =
    ClassFileCodecs
      .classFile
      .encode(cf)
      .require
      .bytes

  // pprint.pprintln(cf)

  Files.write(Paths.get("Foo.class"), bytes.toArray)

}
