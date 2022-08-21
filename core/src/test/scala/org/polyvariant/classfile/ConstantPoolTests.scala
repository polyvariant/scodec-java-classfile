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

import weaver._
import org.polyvariant.classfile._
import scodec.bits.ByteVector

object ConstantPoolTests extends FunSuite {
  val anInt = Constant.IntConstant(ByteVector.empty)
  val aMethodType = Constant.MethodType(ConstantIndex(2))

  val aConstantPool = ConstantPool(
    List(
      anInt,
      Constant.LongConstant(ByteVector.empty, ByteVector.empty),
      aMethodType,
    )
  )

  test("ConstantPool: is 1-indexed") {
    assert(aConstantPool(ConstantIndex(1)) == anInt)
  }

  test("ConstantPool: Long constant shifts indices by one") {
    assert(aConstantPool(ConstantIndex(4)) == aMethodType)
  }

  test("ConstantPool: looking up in the middle of a long returns null") {
    assert(aConstantPool(ConstantIndex(3)) == null)
  }
}
