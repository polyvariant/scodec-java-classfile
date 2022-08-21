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

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import fs2.io.file.Files
import fs2.io.file.Path
import org.polyvariant.classfile.AttributeInfo
import org.polyvariant.classfile.ClassFile
import org.polyvariant.classfile.ClassFileCodecs
import org.polyvariant.classfile.Constant
import org.polyvariant.classfile.ConstantIndex
import scodec.Codec
import scodec.Err
import scodec.bits.ByteVector
import scodec.bits._
import scodec.interop.cats._

import java.nio.charset.StandardCharsets
import scala.reflect.TypeTest

case class ClassModel(
  thisClass: String,
  superClass: String,
  fields: List[FieldModel],
  methods: List[MethodModel],
)

case class FieldModel(name: String, descriptor: String, attributes: List[AttributeModel])
case class MethodModel(name: String, descriptor: String, attributes: List[AttributeModel])

case class AttributeModel(name: String)

object Examples extends IOApp {

  def decode(cf: ClassFile): ClassModel = {
    def resolve(ci: ConstantIndex) = cf.constants(ci)
    def r(ci: ConstantIndex): String =
      resolve(ci) match {
        case Constant.Class(i) => r(i)
        case Constant.Utf8(s) =>
          new String(s.toArray /* this isn't technically utf-8, but let's go */ )
      }

    def decodeAttrs(
      attributes: List[AttributeInfo]
    ): List[AttributeModel] = attributes.map(a => AttributeModel(r(a.nameIndex)))

    ClassModel(
      r(cf.thisClass),
      r(cf.superClass),
      cf.fields
        .map(f =>
          FieldModel(
            r(f.nameIndex),
            r(f.descriptorIndex),
            decodeAttrs(f.attributes),
          )
        ),
      cf.methods
        .map(m =>
          MethodModel(
            r(m.nameIndex),
            r(m.descriptorIndex),
            decodeAttrs(m.attributes),
          )
        ),
    )
  }

  def run(args: List[String]): IO[ExitCode] = Files[IO]
    .readAll(Path(args(0)))
    .compile
    .toVector
    .map { bytes =>
      val bits = ByteVector(bytes).bits

      ClassFileCodecs
        .classFile
        .decode(bits)
        .map(_.value)
        .map(decode)
    }
    .map(_.map(pprint(_)))
    .flatMap(IO.println(_))
    .as(ExitCode.Success)

}
