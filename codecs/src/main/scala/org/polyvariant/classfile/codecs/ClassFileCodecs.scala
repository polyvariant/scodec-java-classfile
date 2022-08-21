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

package org.polyvariant.classfile.codecs

import org.polyvariant.classfile._
import org.polyvariant.classfile.codecs.ScodecUtils._
import scodec.Codec
import scodec.Decoder
import scodec.Encoder
import scodec.Err
import scodec.bits.ByteVector
import scodec.bits._

import java.nio.charset.StandardCharsets

object ClassFileCodecs {

  import scodec.codecs._

  val u1: Codec[Byte] = byte
  val u1Int: Codec[Int] = uint(8)
  val u2: Codec[Int] = uint(16)
  val u4: Codec[Long] = ulong(32)

  private val constantPoolIndex = u2.as[ConstantIndex]

  private val fieldRefCommon =
    ("class index" | constantPoolIndex) ::
      ("name and type index" | constantPoolIndex)

  val methodRef: Codec[Constant.MethodRef] = fieldRefCommon.as[Constant.MethodRef]
  val fieldRef: Codec[Constant.FieldRef] = fieldRefCommon.as[Constant.FieldRef]
  val interfaceMethodRef: Codec[Constant.InterfaceMethodRef] = fieldRefCommon
    .as[Constant.InterfaceMethodRef]

  val nameAndType: Codec[Constant.NameAndType] =
    (("name index" | constantPoolIndex) :: ("descriptor index" | constantPoolIndex))
      .as[Constant.NameAndType]

  val classConstant: Codec[Constant.Class] = ("name index" | constantPoolIndex).as[Constant.Class]

  val utf8Constant: Codec[Constant.Utf8] = ("length" | u2)
    .consume(bytes(_))(_.size.toInt)
    .as[Constant.Utf8]

  val stringConstant: Codec[Constant.StringRef] = ("string index" | constantPoolIndex)
    .as[Constant.StringRef]

  private val numeric = "bytes" | bytes(4)
  private val bigNumeric = ("high bytes" | bytes(4)) :: ("low bytes" | bytes(4))

  val intConstant: Codec[Constant.IntConstant] = numeric.as[Constant.IntConstant]
  val floatConstant: Codec[Constant.FloatConstant] = numeric.as[Constant.FloatConstant]
  val longConstant: Codec[Constant.LongConstant] = bigNumeric.as[Constant.LongConstant]
  val doubleConstant: Codec[Constant.DoubleConstant] = bigNumeric.as[Constant.DoubleConstant]

  val methodType: Codec[Constant.MethodType] = ("descriptor index" | constantPoolIndex)
    .as[Constant.MethodType]

  val methodHandle: Codec[Constant.MethodHandle] =
    (("reference kind" | mappedEnum(
      u1Int,
      MethodReferenceKind.values.map(k => k -> k.ordinal).toMap,
    )) ::
      ("reference index" | constantPoolIndex))
      .as[Constant.MethodHandle]

  val invokeDynamic: Codec[Constant.InvokeDynamic] =
    (("bootstrap method attr index" | u2) ::
      ("name and type index" | constantPoolIndex))
      .as[Constant.InvokeDynamic]

  val constantEntry: Codec[Constant] =
    "constant pool entry" |
      discriminated[Constant]
        .by(u1)
        .typecase(7, classConstant)
        .typecase(9, fieldRef)
        .typecase(10, methodRef)
        .typecase(11, interfaceMethodRef)
        .typecase(8, stringConstant)
        .typecase(3, intConstant)
        .typecase(4, floatConstant)
        .typecase(5, longConstant)
        .typecase(6, doubleConstant)
        .typecase(12, nameAndType)
        .typecase(1, utf8Constant)
        .typecase(15, methodHandle)
        .typecase(16, methodType)
        .typecase(18, invokeDynamic)

  val classAccessFlags: Codec[Set[ClassAccessFlag]] = {
    import ClassAccessFlag._
    "access flags" | masked(
      u2,
      Map(
        Public -> 0x0001,
        Final -> 0x0010,
        Super -> 0x0020,
        Interface -> 0x0200,
        Abstract -> 0x0400,
        Synthetic -> 0x1000,
        Annotation -> 0x2000,
        Enum -> 0x4000,
      ),
    )
  }

  val fieldAccessFlags: Codec[Set[FieldAccessFlag]] = {
    import FieldAccessFlag._
    "access flags" | masked(
      u2,
      Map(
        Public -> 0x0001,
        Private -> 0x0002,
        Protected -> 0x0004,
        Static -> 0x0008,
        Final -> 0x0010,
        Volatile -> 0x0040,
        Transient -> 0x0080,
        Synthetic -> 0x1000,
        Enum -> 0x4000,
      ),
    )
  }

  val methodAccessFlags: Codec[Set[MethodAccessFlag]] = {
    import MethodAccessFlag._
    "access flags" | masked(
      u2,
      Map(
        Public -> 0x0001,
        Private -> 0x0002,
        Protected -> 0x0004,
        Static -> 0x0008,
        Final -> 0x0010,
        Synchronized -> 0x0020,
        Bridge -> 0x0040,
        Varargs -> 0x0080,
        Native -> 0x0100,
        Abstract -> 0x0400,
        Strict -> 0x0800,
        Synthetic -> 0x1000,
      ),
    )
  }

  val attribute: Codec[AttributeInfo] =
    "attribute" | (
      ("name index" | constantPoolIndex) ::
        variableSizeBytesLong(
          "attribute length" | u4,
          "info" | vector(u1),
        ).xmap(ByteVector(_), _.toArray.toVector)
    ).as[AttributeInfo]

  val attributes: Codec[List[AttributeInfo]] =
    "attributes" | listOfN("attributes count" | u2, attribute)

  val fieldInfo: Codec[FieldInfo] =
    "field info" |
      (
        "access flags" | fieldAccessFlags ::
          ("name index" | constantPoolIndex) ::
          ("descriptor index" | constantPoolIndex) ::
          attributes
      ).as[FieldInfo]

  val methodInfo =
    "method info" | (
      ("access flags" | methodAccessFlags) ::
        ("name index" | constantPoolIndex) ::
        ("descriptor index" | constantPoolIndex) ::
        attributes
    ).as[MethodInfo]

  val constantPool: Codec[ConstantPool] = {
    val c = "constant pool count" | u2.xmap(_ - 1, _ + 1)

    weightedN(c, constantEntry)(_.size)
      .xmap(_.toList, _.toVector)
      .xmap(ConstantPool(_), _.constantList)
  }

  val classFile: Codec[ClassFile] =
    (
      ("magic number " | constant(hex"CAFEBABE")) ::
        ("minor version" | u2) ::
        ("major version" | u2) ::
        constantPool ::
        classAccessFlags ::
        ("this class" | constantPoolIndex) ::
        ("super class" | constantPoolIndex) ::
        listOfN(
          "interface count" | u2,
          "interface index" | constantPoolIndex,
        ) ::
        listOfN(
          "field count" | u2,
          fieldInfo,
        ) ::
        listOfN(
          "method count" | u2,
          methodInfo,
        ) ::
        attributes
    ).dropUnits.as[ClassFile]

}
