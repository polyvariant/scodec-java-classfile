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

import cats.implicits._
import org.polyvariant.classfile.ConstantPool
import org.polyvariant.classfile._
import org.polyvariant.classfile.codecs.ClassFileCodecs
import org.polyvariant.classfile.codecs.InstructionCodecs
import pprint.Tree.KeyValue
import pprint.Tree.Literal
import scodec.Codec
import scodec.Err
import scodec.bits.ByteVector
import scodec.bits._

import java.nio.charset.StandardCharsets
import scala.reflect.TypeTest
import java.nio.file.Files
import java.nio.file.Paths

case class ClassModel(
  thisClass: String,
  superClass: String,
  fields: List[FieldModel],
  methods: List[MethodModel],
  attributes: List[AttributeModel],
)

case class FieldModel(name: String, descriptor: String, attributes: List[AttributeModel])
case class MethodModel(name: String, descriptor: String, attributes: List[AttributeModel])

enum AttributeModel {

  def attrName: String =
    this match {
      case _: Code            => "Code"
      case _: LineNumberTable => "LineNumberTable"
      case _: SourceFile      => "SourceFile"
      case _: Signature       => "Signature"
      case u: Unsupported     => u.name
    }

  def attrNameConstant: Constant.Utf8Info = Constant.Utf8Info(attrName)

  case Code(
    maxStack: Int,
    maxLocals: Int,
    code: Vector[Instruction],
    exceptionTable: Vector[ExceptionTableEntry],
    attributes: List[AttributeModel],
  )

  case LineNumberTable(
    entries: Vector[LineNumberTableEntry]
  )

  case SourceFile(name: String)

  case Unsupported(name: String, bytes: ByteVector)
  case Signature(value: String)
}

case class LineNumberTableEntry(startPc: Int, lineNumber: Int)
case class ExceptionTableEntry(startPc: Int, endPc: Int, handlerPc: Int, catchType: Int)

object Examples {

  def main(args: Array[String]): Unit = {
    pprint.apply(42)

    val bytes = Files
      .readAllBytes(Paths.get(args(0)))
    val bits = ByteVector(bytes).bits

    val decoded = ClassFileCodecs
      .classFile
      .decode(bits)
      .map(_.value)
      .map(ExampleCode.decode)
    // .map(_.methods.map(f => f.name -> f.attributes))

    pprint.pprintln(
      decoded
        .toTry
        .get
    )
  }

}

object ExampleCode {

  def decode(cf: ClassFile): ClassModel = {
    import AttributeCodecs._

    extension [C <: Constant: TypeTest[Constant, *]](ci: ConstantIndex[C])
      def resolveIndex: C = cf.constants.resolve[C](ci)

    val superclassName =
      cf.superClass match {
        case ConstantIndex.Zero() => "java/lang/Object (we _are_ Object)"
        case other                => other.resolveIndex.nameIndex.resolveIndex.bytes
      }

    ClassModel(
      cf.thisClass.resolveIndex.nameIndex.resolveIndex.bytes,
      superclassName,
      cf.fields
        .map(f =>
          FieldModel(
            f.nameIndex.resolveIndex.bytes,
            f.descriptorIndex.resolveIndex.bytes,
            AttributeCodecs.decodeAttrs(f.attributes, cf.constants),
          )
        ),
      cf.methods
        .map(m =>
          MethodModel(
            m.nameIndex.resolveIndex.bytes,
            m.descriptorIndex.resolveIndex.bytes,
            AttributeCodecs.decodeAttrs(m.attributes, cf.constants),
          )
        ),
      AttributeCodecs.decodeAttrs(cf.attributes, cf.constants),
    )
  }

}

object AttributeCodecs {
  import scodec.codecs._
  import ClassFileCodecs._
  import InstructionCodecs._

  extension (cp: ConstantPool)

    def resolve[A <: Constant](
      index: ConstantIndex[A]
    )(
      using tt: TypeTest[Constant, A]
    ): A =
      cp(index) match {
        case t: A => t
      }

  def code(cp: ConstantPool): Codec[AttributeModel.Code] =
    (
      ("max stack" | u2) ::
        ("max locals" | u2) ::
        variableSizeBytesLong(
          "code length" | u4,
          "code" | vector(instruction),
        ) ::
        vectorOfN(
          "exception table length" | u2,
          (
            ("start pc" | u2) ::
              ("end pc" | u2) ::
              ("handler pc" | u2) ::
              ("catch type" | u2)
          ).as[ExceptionTableEntry],
        ) ::
        attributes.xmap(decodeAttrs(_, cp), encodeAttrs(_, cp)),
    ).dropUnits.as[AttributeModel.Code]

  val lineNumberTable: Codec[AttributeModel.LineNumberTable] = vectorOfN(
    "line number table length" | u2,
    (("start pc" | u2) ::
      ("line number" | u2)).as[LineNumberTableEntry],
  ).as[AttributeModel.LineNumberTable]

  def sourceFile(
    cp: ConstantPool
  ): Codec[AttributeModel.SourceFile] = ("name index" | constantPoolIndex[Constant.Utf8Info])
    .xmap(
      cp.resolve[Constant.Utf8Info](_).asString,
      s =>
        cp.indexOf[Constant.Utf8Info](Constant.Utf8Info(s))
          .getOrElse(sys.error(s"constant not found: $s (utf8)")),
    )
    .as[AttributeModel.SourceFile]

  def attribute(name: String, cp: ConstantPool): Codec[AttributeModel] = {
    val default = bytes
      .xmap[AttributeModel.Unsupported](AttributeModel.Unsupported(name, _), _.bytes)
      .upcast[AttributeModel]

    Map(
      "Code" -> code(cp).upcast[AttributeModel],
      "LineNumberTable" -> lineNumberTable.upcast[AttributeModel],
      "SourceFile" -> sourceFile(cp).upcast[AttributeModel],
    )
      .get(name)
      .getOrElse(default)
  }

  def encodeAttrs(
    attributes: List[AttributeModel],
    cp: ConstantPool,
  ): List[AttributeInfo] = attributes.map { a =>
    AttributeInfo(
      cp.indexOf(a.attrNameConstant).getOrElse(sys.error(s"constant not found: ${a.attrName}")),
      AttributeCodecs.attribute(a.attrName, cp).encode(a).require.bytes,
    )
  }

  def decodeAttrs(
    attributes: List[AttributeInfo],
    cp: ConstantPool,
  ): List[AttributeModel] = attributes.map { a =>
    val name = cp.resolve[Constant.Utf8Info](a.nameIndex).asString

    attribute(name, cp).decodeValue(a.info.bits).require
  }

}
