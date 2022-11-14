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

import scodec.Codec
import org.polyvariant.classfile._
import scodec.Err

object InstructionCodecs {
  import scodec.bits._
  import scodec.codecs._

  import ClassFileCodecs._

  val arrayType: Codec[ArrayType] = mappedEnum(
    u1, // todo: not sure about this exactly, but it seems to work
    ArrayType.T_BOOLEAN -> 4,
    ArrayType.T_CHAR -> 5,
    ArrayType.T_FLOAT -> 6,
    ArrayType.T_DOUBLE -> 7,
    ArrayType.T_BYTE -> 8,
    ArrayType.T_SHORT -> 9,
    ArrayType.T_INT -> 10,
    ArrayType.T_LONG -> 11,
  )

  val instruction: Codec[Instruction] = {
    import Instruction._
    val localIndex = ("local variable array index" | u1).as[LocalVariableIndex]

    val offset = ("offset" | u2).as[Offset]
    val offsetWide = ("offset" | u4).as[OffsetWide]

    val anyConstant = constantPoolIndex[Constant]
    discriminated[Instruction]
      .by(u1)
      .typecase(0x32, localIndex.as[dload])
      .singleton(0x53, aastore)
      .singleton(0x01, aconst_null)
      .typecase(0x19, localIndex.as[aload])
      .singleton(0x2a, aload_0)
      .singleton(0x2b, aload_1)
      .singleton(0x2c, aload_2)
      .singleton(0x2d, aload_3)
      .typecase(0xbd, anyConstant.as[anewarray])
      .singleton(0xb0, areturn)
      .singleton(0xbe, arraylength)
      .typecase(0x3a, localIndex.as[astore])
      .singleton(0x4b, astore_0)
      .singleton(0x4c, astore_1)
      .singleton(0x4d, astore_2)
      .singleton(0x4e, astore_3)
      .singleton(0xbf, athrow)
      .singleton(0x33, baload)
      .singleton(0x54, bastore)
      .typecase(0x10, byte.as[bipush])
      .singleton(0x34, caload)
      .singleton(0x55, castore)
      .typecase(0xc0, anyConstant.as[checkcast])
      .singleton(0x90, d2f)
      .singleton(0x8e, d2i)
      .singleton(0x8f, d2l)
      .singleton(0x63, dadd)
      .singleton(0x31, daload)
      .singleton(0x52, dastore)
      .singleton(0x98, dcmpg)
      .singleton(0x97, dcmpl)
      .singleton(0x0e, dconst_0)
      .singleton(0x0f, dconst_1)
      .singleton(0x6f, ddiv)
      .typecase(0x18, localIndex.as[dload])
      .singleton(0x26, dload_0)
      .singleton(0x27, dload_1)
      .singleton(0x28, dload_2)
      .singleton(0x29, dload_3)
      .singleton(0x6b, dmul)
      .singleton(0x77, dneg)
      .singleton(0x73, drem)
      .singleton(0xaf, dreturn)
      .typecase(0x39, localIndex.as[dstore])
      .singleton(0x47, dstore_0)
      .singleton(0x48, dstore_1)
      .singleton(0x49, dstore_2)
      .singleton(0x4a, dstore_3)
      .singleton(0x67, dsub)
      .singleton(0x59, dup)
      .singleton(0x5a, dup_x1)
      .singleton(0x5b, dup_x2)
      .singleton(0x5c, dup2)
      .singleton(0x5d, dup2_x1)
      .singleton(0x5e, dup2_x2)
      .singleton(0x8d, f2d)
      .singleton(0x8b, f2i)
      .singleton(0x8c, f2l)
      .singleton(0x62, fadd)
      .singleton(0x30, faload)
      .singleton(0x51, fastore)
      .singleton(0x96, fcmpg)
      .singleton(0x95, fcmpl)
      .singleton(0x0b, fconst_0)
      .singleton(0x0c, fconst_1)
      .singleton(0x0d, fconst_2)
      .singleton(0x6e, fdiv)
      .typecase(0x17, localIndex.as[fload])
      .singleton(0x22, fload_0)
      .singleton(0x23, fload_1)
      .singleton(0x24, fload_2)
      .singleton(0x25, fload_3)
      .singleton(0x6a, fmul)
      .singleton(0x76, fneg)
      .singleton(0x72, frem)
      .singleton(0xae, freturn)
      .typecase(0x38, localIndex.as[fstore])
      .singleton(0x43, fstore_0)
      .singleton(0x44, fstore_1)
      .singleton(0x45, fstore_2)
      .singleton(0x46, fstore_3)
      .singleton(0x66, fsub)
      .typecase(0xb4, anyConstant.as[getfield])
      .typecase(0xb2, anyConstant.as[getstatic])
      .typecase(0xa7, offset.as[goto])
      .typecase(0xc8, offsetWide.as[goto_w])
      .singleton(0x91, i2b)
      .singleton(0x92, i2c)
      .singleton(0x87, i2d)
      .singleton(0x86, i2f)
      .singleton(0x85, i2l)
      .singleton(0x93, i2s)
      .singleton(0x60, iadd)
      .singleton(0x2e, iaload)
      .singleton(0x7e, iand)
      .singleton(0x4f, iastore)
      .singleton(0x02, iconst_m1)
      .singleton(0x03, iconst_0)
      .singleton(0x04, iconst_1)
      .singleton(0x05, iconst_2)
      .singleton(0x06, iconst_3)
      .singleton(0x07, iconst_4)
      .singleton(0x08, iconst_5)
      .singleton(0x6c, idiv)
      .typecase(0xa5, offset.as[if_acmpeq])
      .typecase(0xa6, offset.as[if_acmpne])
      .typecase(0x9f, offset.as[if_icmpeq])
      .typecase(0xa0, offset.as[if_icmpne])
      .typecase(0xa1, offset.as[if_icmplt])
      .typecase(0xa2, offset.as[if_icmpge])
      .typecase(0xa3, offset.as[if_icmpgt])
      .typecase(0xa4, offset.as[if_icmple])
      .typecase(0x99, offset.as[ifeq])
      .typecase(0x9a, offset.as[ifne])
      .typecase(0x9b, offset.as[iflt])
      .typecase(0x9c, offset.as[ifge])
      .typecase(0x9d, offset.as[ifgt])
      .typecase(0x9e, offset.as[ifle])
      .typecase(0xc7, offset.as[ifnonnull])
      .typecase(0xc6, offset.as[ifnull])
      .typecase(0x84, (localIndex :: ("const" | byte)).as[iinc])
      .typecase(0x15, localIndex.as[iload])
      .singleton(0x1a, iload_0)
      .singleton(0x1b, iload_1)
      .singleton(0x1c, iload_2)
      .singleton(0x1d, iload_3)
      .singleton(0xfe, imul)
      .singleton(0x74, ineg)
      .typecase(0xc1, anyConstant.as[instanceof])
      .typecase(0xba, (anyConstant :: constant(hex"0000")).dropUnits.as[invokedynamic])
      .typecase(
        0xb9,
        (anyConstant :: ("count" | u1) :: constant(hex"0")).dropUnits.as[invokeinterface],
      )
      .typecase(0xb7, anyConstant.as[invokespecial])
      .typecase(0xb8, anyConstant.as[invokestatic])
      .typecase(0xb6, anyConstant.as[invokevirtual])
      .singleton(0x80, ior)
      .singleton(0x70, irem)
      .singleton(0xac, ireturn)
      .singleton(0x78, ishl)
      .singleton(0x7a, ishr)
      .typecase(0x36, localIndex.as[istore])
      .singleton(0x3b, istore_0)
      .singleton(0x3c, istore_1)
      .singleton(0x3d, istore_2)
      .singleton(0x3e, istore_3)
      .singleton(0x64, isub)
      .singleton(0x7c, iushr)
      .singleton(0x82, ixor)
      .typecase(0xa8, offset.as[jsr])
      .typecase(0xc9, offsetWide.as[jsr_w])
      .singleton(0x8a, l2d)
      .singleton(0x89, l2f)
      .singleton(0x88, l2i)
      .singleton(0x61, ladd)
      .singleton(0x2f, laload)
      .singleton(0x7f, land)
      .singleton(0x50, lastore)
      .singleton(0x94, lcmp)
      .singleton(0x09, lconst_0)
      .singleton(0x0a, lconst_1)
      .typecase(0x12, constantPoolIndexNarrow.as[ldc])
      .typecase(0x13, anyConstant.as[ldc_w])
      .typecase(0x14, anyConstant.as[ldc2_w])
      .singleton(0x6d, ldiv)
      .typecase(0x16, localIndex.as[lload])
      .singleton(0x1e, lload_0)
      .singleton(0x1f, lload_1)
      .singleton(0x20, lload_2)
      .singleton(0x21, lload_3)
      .singleton(0x69, lmul)
      .singleton(0x75, lneg)
      .typecase(0xab, fail(Err("lookupswitch not supported")))
      .singleton(0x81, lor)
      .singleton(0x71, lrem)
      .singleton(0xad, lreturn)
      .singleton(0x79, lshl)
      .singleton(0x7b, lshr)
      .typecase(0x37, localIndex.as[lstore])
      .singleton(0x3f, lstore_0)
      .singleton(0x40, lstore_1)
      .singleton(0x41, lstore_2)
      .singleton(0x42, lstore_3)
      .singleton(0x65, lsub)
      .singleton(0x7d, lushr)
      .singleton(0x83, lxor)
      .singleton(0xc2, monitorenter)
      .singleton(0xc3, monitorexit)
      .typecase(0xc5, (anyConstant :: u1).as[multianewarray])
      .typecase(0xbb, anyConstant.as[_new])
      .typecase(0xbc, arrayType.as[newarray])
      .singleton(0x00, nop)
      .singleton(0x57, pop)
      .singleton(0x58, pop2)
      .typecase(0xb5, anyConstant.as[putfield])
      .typecase(0xb3, anyConstant.as[putstatic])
      .typecase(0xa9, localIndex.as[ret])
      .singleton(0xb1, _return)
      .singleton(0x35, saload)
      .singleton(0x56, sastore)
      .typecase(0x11, u2.as[sipush])
      .singleton(0x5f, swap)
      .typecase(0xaa, fail(Err("tableswitch not supported")))
      .typecase(0xc4, fail(Err("wide not supported")))
  }

}
