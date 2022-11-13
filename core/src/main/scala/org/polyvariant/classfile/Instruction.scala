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

type AnyConstantIndex = ConstantIndex[Constant]

enum Instruction {
  case aaload
  case aastore
  case aconst_null
  case aload(index: LocalVariableIndex)
  case aload_0, aload_1, aload_2, aload_3
  case anewarray(index: AnyConstantIndex)
  case areturn
  case arraylength
  case astore(index: LocalVariableIndex)
  case astore_0, astore_1, astore_2, astore_3
  case athrow
  case baload
  case bastore
  case bipush(byte: Byte)
  case caload
  case castore
  case checkcast(index: AnyConstantIndex)
  case d2f
  case d2i
  case d2l
  case dadd
  case daload
  case dastore
  case dcmpg, dcmpl
  case dconst_0, dconst_1
  case ddiv
  case dload(index: LocalVariableIndex)
  case dload_0, dload_1, dload_2, dload_3
  case dmul
  case dneg
  case drem
  case dreturn
  case dstore(index: LocalVariableIndex)
  case dstore_0, dstore_1, dstore_2, dstore_3
  case dsub
  case dup, dup_x1, dup_x2
  case dup2, dup2_x1, dup2_x2
  case f2d
  case f2i
  case f2l
  case fadd
  case faload
  case fastore
  case fcmpg, fcmpl
  case fconst_0, fconst_1, fconst_2
  case fdiv
  case fload(index: LocalVariableIndex)
  case fload_0, fload_1, fload_2, fload_3
  case fmul
  case fneg
  case frem
  case freturn
  case fstore(index: LocalVariableIndex)
  case fstore_0, fstore_1, fstore_2, fstore_3
  case fsub
  case getfield(index: AnyConstantIndex)
  case getstatic(index: AnyConstantIndex)
  case goto(offset: Offset)
  case goto_w(offset: OffsetWide)
  case i2b
  case i2c
  case i2d
  case i2f
  case i2l
  case i2s
  case iadd
  case iaload
  case iand
  case iastore
  case iconst_m1, iconst_0, iconst_1, iconst_2, iconst_3, iconst_4, iconst_5
  case idiv
  case if_acmpeq(offset: Offset)
  case if_acmpne(offset: Offset)
  case if_icmpeq(offset: Offset)
  case if_icmpne(offset: Offset)
  case if_icmplt(offset: Offset)
  case if_icmpge(offset: Offset)
  case if_icmpgt(offset: Offset)
  case if_icmple(offset: Offset)
  case ifeq(offset: Offset)
  case ifne(offset: Offset)
  case iflt(offset: Offset)
  case ifge(offset: Offset)
  case ifgt(offset: Offset)
  case ifle(offset: Offset)
  case ifnonnull(offset: Offset)
  case ifnull(offset: Offset)
  case iinc(index: LocalVariableIndex, const: Byte)
  case iload(index: LocalVariableIndex)
  case iload_0, iload_1, iload_2, iload_3
  case imul
  case ineg
  case instanceof(index: AnyConstantIndex)
  case invokedynamic(index: AnyConstantIndex)
  case invokeinterface(index: AnyConstantIndex, count: Short)
  case invokespecial(index: AnyConstantIndex)
  case invokestatic(index: AnyConstantIndex)
  case invokevirtual(index: AnyConstantIndex)
  case ior
  case irem
  case ireturn
  case ishl
  case ishr
  case istore(index: LocalVariableIndex)
  case istore_0, istore_1, istore_2, istore_3
  case isub
  case iushr
  case ixor
  case jsr(offset: Offset)
  case jsr_w(offset: OffsetWide)
  case l2d
  case l2f
  case l2i
  case ladd
  case laload
  case land
  case lastore
  case lcmp
  case lconst_0, lconst_1
  case ldc(index: ConstantIndexNarrow)
  case ldc_w(index: AnyConstantIndex)
  case ldc2_w(index: AnyConstantIndex)
  case ldiv
  case lload(index: LocalVariableIndex)
  case lload_0, lload_1, lload_2, lload_3
  case lmul
  case lneg
  // case lookupswitch
  case lor
  case lrem
  case lreturn
  case lshl
  case lshr
  case lstore(index: LocalVariableIndex)
  case lstore_0, lstore_1, lstore_2, lstore_3
  case lsub
  case lushr
  case lxor
  case monitorenter
  case monitorexit
  case multianewarray(index: AnyConstantIndex, dimensions: Short)
  case _new(index: AnyConstantIndex)
  case newarray(atype: ArrayType)
  case nop
  case pop
  case pop2
  case putfield(index: AnyConstantIndex)
  case putstatic(index: AnyConstantIndex)
  case ret(index: LocalVariableIndex)
  case _return
  case saload
  case sastore
  case sipush(short: Int)
  case swap
  // case tableswitch
  // case wide
}

case class Offset(value: Int)
case class OffsetWide(value: Long)
case class LocalVariableIndex(value: Short)

enum ArrayType {
  case T_BOOLEAN, T_CHAR, T_FLOAT, T_DOUBLE, T_BYTE, T_SHORT, T_INT, T_LONG
}
