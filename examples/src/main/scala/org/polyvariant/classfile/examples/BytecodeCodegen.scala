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

import cats.effect.IOApp
import cats.effect.IO
import org.polyvariant.classfile.ClassFile
import org.polyvariant.classfile.ConstantPool
import org.polyvariant.classfile.ClassAccessFlag
import org.polyvariant.classfile.Constant
import org.polyvariant.classfile.ConstantIndex
import org.polyvariant.classfile.MethodInfo
import org.polyvariant.classfile.MethodAccessFlag
import org.polyvariant.classfile.AttributeInfo
import scodec.bits._
import fs2.io.file.Files
import fs2.io.file.Path
import org.polyvariant.classfile.codecs.ClassFileCodecs
import org.polyvariant.classfile.Instruction
import org.polyvariant.classfile.examples.Examples.AttributeCodecs
import cats.effect.kernel.Ref
import cats.Functor
import cats.implicits._
import cats.data.State
import cats.data.Chain
import java.nio.file.Paths
import java.io.DataInput
import java.io.DataInputStream
import java.io.FileInputStream

object BytecodeCodegen extends IOApp.Simple {

  trait PoolOps[F[_]] {
    def add(c: Constant): F[ConstantIndex]
    def addUtf8String(s: T): F[ConstantIndex] = add(Constant.Utf8Info(s.render))
    def addClassInfo(c: ConstantIndex) = add(Constant.ClassInfo(c))
  }

  object PoolOps {

    type PoolOp[A] = State[ConstantPool, A]

    val stateInstance: PoolOps[PoolOp] =
      new PoolOps[PoolOp] {

        def add(c: Constant): PoolOp[ConstantIndex] = State.get[ConstantPool].flatMap { old =>
          old.indexOf(c) match {
            case None =>
              val nextIndex = ConstantIndex(old.declaredSize + c.size)
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

  case class T private (sym: String) extends Dynamic {
    def selectDynamic(name: String): T = copy(s"$sym/$name")
    def render: String = sym
    def obj: T = copy(s"L$sym;")
    def array: T = copy(s"[$sym")
    def method(returns: T): T = copy(s"($sym)${returns.render}")
  }

  // Type descriptor DSL
  object T extends Dynamic {
    def string(s: String): T = new T(s)
    def selectDynamic(name: String): T = new T(name)
  }

  def mkSystemOut(pool: PoolOps[PoolOps.PoolOp]) =
    for {
      system <- pool
        .addUtf8String(T.java.lang.System)
        .flatMap(pool.addClassInfo)
      out <- pool.addUtf8String(T.out)
      printStream <- pool.addUtf8String(T.java.io.PrintStream.obj)
      nat <- pool.add(Constant.NameAndTypeInfo(out, printStream))
      fieldRefInfo <- pool.add(Constant.FieldRefInfo(system, nat))
    } yield fieldRefInfo

  def mkPrintln(pool: PoolOps[PoolOps.PoolOp]) =
    for {
      printStream <- pool.addUtf8String(T.java.io.PrintStream).flatMap(pool.addClassInfo)
      println <- pool.addUtf8String(T.println)
      descriptor <- pool.addUtf8String(T.java.lang.String.obj.method(T.V))
      nat <- pool.add(Constant.NameAndTypeInfo(println, descriptor))
      methodRefInfo <- pool.add(Constant.MethodRefInfo(printStream, nat))
    } yield methodRefInfo

  def mkPrintlnCall(
    pool: PoolOps[PoolOps.PoolOp],
    text: String,
  ): PoolOps.PoolOp[(Vector[Instruction], Int)] =
    for {
      stringIndex <- pool
        .addUtf8String(T.string(text))
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
      mainNameIndex <- pool.addUtf8String(T.main)
      voidDescriptorIndex <- pool.addUtf8String(T.java.lang.String.obj.array.method(T.V))
      _ <- pool.addUtf8String(T.string("Code"))
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
      thisClass <- pool.addUtf8String(T.string("Foo")).flatMap(pool.addClassInfo)
      superClass <- pool.addUtf8String(T.java.lang.Object).flatMap(pool.addClassInfo)
      mainMethod <- mkMainMethod(pool)
      _ <- pool.addUtf8String(T.string("SYNTHETIC.java"))
      _ <- pool.addUtf8String(T.string("SourceFile"))
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

  def run: IO[Unit] =
    fs2
      .Stream
      .chunk(fs2.Chunk.byteVector(bytes))
      .through(Files[IO].writeAll(Path("Foo.class")))
      .compile
      .drain

}
