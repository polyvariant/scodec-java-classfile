//> using lib "org.scodec::scodec-core:2.2.0"
//> using lib "org.scodec::scodec-cats:1.1.0"
//> using plugin "org.polyvariant:::better-tostring:0.3.16"
//> using lib "co.fs2::fs2-io:3.2.12"
//> using lib "com.lihaoyi::pprint:0.7.3"
//> using scala "3.1.3"
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
import scodec.Encoder
import scodec.Decoder
import cats.data.Chain

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

object ClassFileCodecs {

  import scodec.codecs._

  private def masked[A](range: Codec[Int], valuesWithMasks: Map[A, Int]): Codec[Set[A]] =
    range
      .imap { v =>
        valuesWithMasks.collect {
          case (flag, mask) if (mask & v) == mask => flag
        }.toSet
      } { flags =>
        flags.map(flag => valuesWithMasks(flag)).foldLeft(0)(_ | _)
      }

  private val u1 = byte
  private val u1Int = uint(8)
  private val u2 = uint(16)
  private val u4 = ulong(32)

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
        ).imap(ByteVector(_))(_.toArray.toVector)
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

  def weightedN[A](n: Codec[Int], item: Codec[A])(weight: A => Int): Codec[Vector[A]] =
    "weightedN" | {

      def go(remaining: Int): Decoder[List[A]] =
        remaining match {
          case 0 => Decoder.pure(Nil)
          case n =>
            item
              .flatMap { a =>
                go(remaining - weight(a)).map(a :: _)
              }
        }

      logToStdOut(n).consume { size =>
        val encoder: Encoder[Vector[A]] = vector(item)

        val decoder = go(size).map(_.toVector)

        Codec(encoder, decoder)
      }(_.foldMap(weight))
    }

  val constantPool: Codec[ConstantPool] = {
    val c = "constant pool count" | u2.imap(_ - 1)(_ + 1)

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
