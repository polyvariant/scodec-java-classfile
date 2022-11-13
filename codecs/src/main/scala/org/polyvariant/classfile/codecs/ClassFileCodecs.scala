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

  val u1: Codec[Short] = ushort(8)
  val u2: Codec[Int] = uint(16)
  val u4: Codec[Long] = ulong(32)

  def constantPoolIndex[T <: Constant]: Codec[ConstantIndex[T]] = u2.as[ConstantIndex[T]]

  val constantPoolIndexNarrow: Codec[ConstantIndexNarrow] = u1.as[ConstantIndexNarrow]

  private val fieldRefCommon =
    ("class index" | constantPoolIndex[Constant.ClassInfo]) ::
      ("name and type index" | constantPoolIndex[Constant])

  val methodRef: Codec[Constant.MethodRefInfo] = fieldRefCommon.as[Constant.MethodRefInfo]
  val fieldRef: Codec[Constant.FieldRefInfo] = fieldRefCommon.as[Constant.FieldRefInfo]
  val interfaceMethodRef: Codec[Constant.InterfaceMethodRefInfo] = fieldRefCommon
    .as[Constant.InterfaceMethodRefInfo]

  val nameAndType: Codec[Constant.NameAndTypeInfo] =
    (("name index" | constantPoolIndex[Constant.Utf8Info]) ::
      ("descriptor index" | constantPoolIndex[Constant.Utf8Info]))
      .as[Constant.NameAndTypeInfo]

  val classConstant: Codec[Constant.ClassInfo] =
    ("name index" | constantPoolIndex[Constant.Utf8Info])
      .as[Constant.ClassInfo]

  // def encodeString(s: String) = {
  // val charArray = s
  //   .chars()
  //   .toArray()
  // charArray
  //   .map {
  //     case b if ('\u0001'.toInt to '\u007F'.toInt).contains(b) =>
  //       (bin"0" ++ ubyte(7).encode(b.toByte).require).bytes
  //     case b if b == '\u0000'.toInt || ('\u0080'.toInt to '\u07FF'.toInt).contains(b) =>
  //       sys.error(s"unsupported #1 ($s): $b")
  //     case b if ('\u0800'.toInt to '\uFFFF'.toInt).contains(b) =>
  //       sys.error(s"unsupported #2 ($s): $b")
  //   }
  //   .foldLeft(ByteVector.empty)(_ ++ _)
  // }

  val utf8Constant: Codec[Constant.Utf8Info] = ("length" | u2)
    .consume(bytes(_))(_.size.toInt)
    // this will need some attention to ensure full compatibility
    .xmap(bytes => new String(bytes.toArray), s => ByteVector(s.getBytes()))
    .as[Constant.Utf8Info]

  val stringConstant: Codec[Constant.StringInfo] =
    ("string index" | constantPoolIndex[Constant.Utf8Info])
      .as[Constant.StringInfo]

  private val numeric = "bytes" | bytes(4)
  private val bigNumeric = ("high bytes" | bytes(4)) :: ("low bytes" | bytes(4))

  val intConstant: Codec[Constant.IntegerInfo] = numeric.as[Constant.IntegerInfo]
  val floatConstant: Codec[Constant.FloatInfo] = numeric.as[Constant.FloatInfo]
  val longConstant: Codec[Constant.LongInfo] = bigNumeric.as[Constant.LongInfo]
  val doubleConstant: Codec[Constant.DoubleInfo] = bigNumeric.as[Constant.DoubleInfo]

  val methodType: Codec[Constant.MethodTypeInfo] =
    ("descriptor index" | constantPoolIndex[Constant.NameAndTypeInfo])
      .as[Constant.MethodTypeInfo]

  val methodHandle: Codec[Constant.MethodHandleInfo] =
    (("reference kind" | mappedEnum(
      u1,
      MethodReferenceKind.values.map(k => k -> k.ordinal.toShort).toMap,
    )) ::
      ("reference index" | constantPoolIndex[Constant]))
      .as[Constant.MethodHandleInfo]

  private val dynamicCommon =
    ("bootstrap method attr index" | u2) ::
      ("name and type index" | constantPoolIndex[Constant.NameAndTypeInfo])

  val dynamic: Codec[Constant.DynamicInfo] = dynamicCommon.as[Constant.DynamicInfo]

  val invokeDynamic: Codec[Constant.InvokeDynamicInfo] = dynamicCommon
    .as[Constant.InvokeDynamicInfo]

  val module: Codec[Constant.ModuleInfo] = ("name index" | constantPoolIndex[Constant.Utf8Info])
    .as[Constant.ModuleInfo]

  val pkg: Codec[Constant.PackageInfo] = ("name index" | constantPoolIndex[Constant.Utf8Info])
    .as[Constant.PackageInfo]

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
        .typecase(17, dynamic)
        .typecase(18, invokeDynamic)
        .typecase(19, module)
        .typecase(20, pkg)

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
      ("name index" | constantPoolIndex[Constant.Utf8Info]) ::
        variableSizeBytesLong(
          "attribute length" | u4,
          "info" | bytes,
        )
    ).as[AttributeInfo]

  val attributes: Codec[List[AttributeInfo]] =
    "attributes" | listOfN("attributes count" | u2, attribute)

  val fieldInfo: Codec[FieldInfo] =
    "field info" |
      (
        "access flags" | fieldAccessFlags ::
          ("name index" | constantPoolIndex[Constant.Utf8Info]) ::
          ("descriptor index" | constantPoolIndex[Constant.Utf8Info]) ::
          attributes
      ).as[FieldInfo]

  val methodInfo =
    "method info" | (
      ("access flags" | methodAccessFlags) ::
        ("name index" | constantPoolIndex[Constant.Utf8Info]) ::
        ("descriptor index" | constantPoolIndex[Constant.Utf8Info]) ::
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
        ("this class" | constantPoolIndex[Constant.ClassInfo]) ::
        ("super class" | constantPoolIndex[Constant.ClassInfo]) ::
        listOfN(
          "interface count" | u2,
          "interface index" | constantPoolIndex[Constant.ClassInfo],
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
