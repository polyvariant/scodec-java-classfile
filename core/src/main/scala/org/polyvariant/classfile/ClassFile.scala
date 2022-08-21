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

import java.nio.charset.StandardCharsets

case class ClassFile(
  minorVersion: Int,
  majorVersion: Int,
  constants: ConstantPool,
  accessFlags: Set[ClassAccessFlag],
  thisClass: ConstantIndex,
  superClass: ConstantIndex,
  interfaces: List[ConstantIndex],
  fields: List[FieldInfo],
  methods: List[MethodInfo],
  attributes: List[AttributeInfo],
)

final case class ConstantPool private (private val constants: Array[Constant | Null])
  extends AnyVal {
  // ConstantIndex is 1-based, so we subtract 1
  def apply(index: ConstantIndex): Constant = constants(index.value - 1)

  // Size declared in length byte
  def declaredSize: Int = constants.length

  // Amount of actual constants inside
  def realSize: Int = constants.count(_ != null)

  def constantList: List[Constant] = constants.toList.collect { case c: Constant => c }
}

object ConstantPool {

  def apply(constants: List[Constant]): ConstantPool = apply {
    constants.flatMap { c =>
      List(c) ++ List.fill(c.size - 1)(null)
    }.toArray
  }

}

case class ConstantIndex(value: Int)

enum Constant {

  def size: Int =
    this match {
      case _: LongConstant | _: DoubleConstant => 2
      case _                                   => 1
    }

  case Class(nameIndex: ConstantIndex)
  case FieldRef(classIndex: ConstantIndex, nameAndTypeIndex: ConstantIndex)
  case MethodRef(classIndex: ConstantIndex, nameAndTypeIndex: ConstantIndex)
  case InterfaceMethodRef(classIndex: ConstantIndex, nameAndTypeIndex: ConstantIndex)
  case StringRef(stringIndex: ConstantIndex)
  case IntConstant(bytes: ByteVector)
  case FloatConstant(bytes: ByteVector)
  case LongConstant(highBytes: ByteVector, lowBytes: ByteVector)
  case DoubleConstant(highBytes: ByteVector, lowBytes: ByteVector)
  case NameAndType(nameIndex: ConstantIndex, descriptorIndex: ConstantIndex)
  case Utf8(bytes: ByteVector)
  case MethodHandle(referenceType: MethodReferenceKind, referenceIndex: ConstantIndex)
  case MethodType(descriptorIndex: ConstantIndex)
  case InvokeDynamic(bootstrapMethodAttrIndex: Int, nameAndTypeIndex: ConstantIndex)
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
  nameIndex: ConstantIndex,
  descriptorIndex: ConstantIndex,
  attributes: List[AttributeInfo],
)

case class AttributeInfo(
  nameIndex: ConstantIndex,
  info: ByteVector,
)

case class MethodInfo(
  accessFlags: Set[MethodAccessFlag],
  nameIndex: ConstantIndex,
  descriptorIndex: ConstantIndex,
  attributes: List[AttributeInfo],
)