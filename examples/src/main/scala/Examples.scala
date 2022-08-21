import fs2.io.file.Files

import cats.effect.IO
import fs2.io.file.Path
import cats.effect.IOApp
import scodec.bits.ByteVector
import scodec.bits._
import scodec.Codec
import cats.implicits._
import scodec.interop.cats._
import java.nio.charset.StandardCharsets
import cats.effect.ExitCode
import scodec.Err
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

object Main extends IOApp {

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
