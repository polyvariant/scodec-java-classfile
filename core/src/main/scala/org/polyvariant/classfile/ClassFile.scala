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

package org.polyvariant.classfile

import scodec.bits.ByteVector
import scodec.bits._

case class ClassFile(
  minorVersion: Int,
  majorVersion: Int,
  constants: ConstantPool,
  accessFlags: Set[ClassAccessFlag],
  thisClass: ConstantIndex[Constant.ClassInfo],
  superClass: ConstantIndex[Constant.ClassInfo],
  interfaces: List[ConstantIndex[Constant.ClassInfo]],
  fields: List[FieldInfo],
  methods: List[MethodInfo],
  attributes: List[AttributeInfo],
)

object raw {

  type ??? = Nothing

  case class ClassFile(
    minorVersion: Int,
    majorVersion: Int,
    constants: ConstantPool,
    accessFlags: Set[ClassAccessFlag],
    thisClass: ???,
    superClass: ???,
    interfaces: List[???],
    fields: List[FieldInfo],
    methods: List[MethodInfo],
    attributes: List[AttributeInfo],
  )

}

final case class ConstantPool private (private val constants: Array[Constant | Null])
  extends AnyVal {
  // ConstantIndex is 1-based, so we subtract 1
  def apply[A <: Constant](index: ConstantIndex[A]): Constant = constants(index.value - 1)

  // Size declared in length byte
  def declaredSize: Int = constants.length

  // Amount of actual constants inside
  def realSize: Int = constants.count(_ != null)

  def constantList: List[Constant] = constants.toList.collect { case c: Constant => c }

  def indexOf[C <: Constant](constant: C): Option[ConstantIndex[C]] =
    constants.indexOf(constant) match {
      case -1 => None
      case i  => Some(ConstantIndex(i + 1))
    }

  override def equals(another: Any): Boolean =
    another match {
      case that: ConstantPool => this.constantList == that.constantList
      case _                  => false
    }

  def append(c: Constant): ConstantPool = ConstantPool.apply(constantList.appended(c))

}

object ConstantPool {

  def apply(constants: List[Constant]): ConstantPool =
    new ConstantPool(
      constants.flatMap { c =>
        List(c) ++ List.fill(c.size - 1)(null)
      }.toArray
    )

}

case class ConstantIndex[C <: Constant](value: Int) {

  def toNarrowEither: Either[this.type, ConstantIndexNarrow] =
    if (value.isValidShort)
      Right(ConstantIndexNarrow(value.toShort))
    else
      Left(this)

}

object ConstantIndex {

  object Zero {
    def unapply(i: ConstantIndex[?]): Boolean = i.value == 0
  }

  val init: ConstantIndex[Nothing] = ConstantIndex(1)
}

case class ConstantIndexNarrow(value: Short)

enum Constant {

  def size: Int =
    this match {
      case _: LongInfo | _: DoubleInfo => 2
      case _                           => 1
    }

  case ClassInfo(nameIndex: ConstantIndex[Constant.Utf8Info])

  case FieldRefInfo(
    classIndex: ConstantIndex[Constant.ClassInfo],
    nameAndTypeIndex: ConstantIndex[Constant],
  )

  case MethodRefInfo(
    classIndex: ConstantIndex[Constant.ClassInfo],
    nameAndTypeIndex: ConstantIndex[Constant],
  )

  case InterfaceMethodRefInfo(
    classIndex: ConstantIndex[Constant.ClassInfo],
    nameAndTypeIndex: ConstantIndex[Constant],
  )

  case StringInfo(stringIndex: ConstantIndex[Constant.Utf8Info])
  case IntegerInfo(bytes: ByteVector)
  case FloatInfo(bytes: ByteVector)
  case LongInfo(highBytes: ByteVector, lowBytes: ByteVector)
  case DoubleInfo(highBytes: ByteVector, lowBytes: ByteVector)

  case NameAndTypeInfo(
    nameIndex: ConstantIndex[Constant.Utf8Info],
    descriptorIndex: ConstantIndex[Constant.Utf8Info],
  )

  case Utf8Info(bytes: String)
  case MethodHandleInfo(referenceType: MethodReferenceKind, referenceIndex: ConstantIndex[Constant])
  case MethodTypeInfo(descriptorIndex: ConstantIndex[Constant.NameAndTypeInfo])

  case DynamicInfo(
    bootstrapMethodAttrIndex: Int,
    nameAndTypeIndex: ConstantIndex[Constant.NameAndTypeInfo],
  )

  case InvokeDynamicInfo(
    bootstrapMethodAttrIndex: Int,
    nameAndTypeIndex: ConstantIndex[Constant.NameAndTypeInfo],
  )

  case ModuleInfo(nameIndex: ConstantIndex[Constant.Utf8Info])
  case PackageInfo(nameIndex: ConstantIndex[Constant.Utf8Info])
}

object Constant {
  extension (utf8: Utf8Info) def asString: String = utf8.bytes

}

enum ClassAccessFlag {
  case Public, Final, Super, Interface, Abstract, Synthetic, Annotation, Enum
}

enum FieldAccessFlag {
  case Public, Private, Protected, Static, Final, Volatile, Transient, Synthetic, Enum
}

enum MethodAccessFlag {
  case Public, Private, Protected, Static, Final, Synchronized, Bridge, Varargs, Native, Abstract,
    Strict, Synthetic
}

enum MethodReferenceKind {
  case GetField, GetStatic, PutField, PutStatic, InvokeVirtual, InvokeStatic, InvokeSpecial,
    NewInvokeSpecial, InvokeInterface
}

case class FieldInfo(
  accessFlags: Set[FieldAccessFlag],
  nameIndex: ConstantIndex[Constant.Utf8Info],
  descriptorIndex: ConstantIndex[Constant.Utf8Info],
  attributes: List[AttributeInfo],
)

case class MethodInfo(
  accessFlags: Set[MethodAccessFlag],
  nameIndex: ConstantIndex[Constant.Utf8Info],
  descriptorIndex: ConstantIndex[Constant.Utf8Info],
  attributes: List[AttributeInfo],
)

case class AttributeInfo(
  nameIndex: ConstantIndex[Constant.Utf8Info],
  info: ByteVector,
)
