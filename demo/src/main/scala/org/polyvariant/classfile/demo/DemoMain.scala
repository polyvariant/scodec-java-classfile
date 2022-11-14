package org.polyvariant.classfile.demo

import org.scalajs.dom._
import org.scalajs.dom.HTMLInputElement
import scala.scalajs.js.typedarray.Uint8Array
import scalatags.JsDom.all._
import scodec.bits.ByteVector
import org.polyvariant.classfile.codecs.ClassFileCodecs
import org.polyvariant.classfile.examples.ExampleCode
import org.polyvariant.classfile.examples.MethodModel
import scalatags.JsDom.TypedTag
import org.polyvariant.classfile.examples.AttributeModel
import scala.util.Failure
import scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.html.TableRow
import org.polyvariant.classfile.examples.AttributeModel
import org.polyvariant.classfile.examples.ClassModel
import org.polyvariant.classfile.ClassFile

@js.native
@JSGlobal
object Prism extends js.Any {
  def highlightAll(): js.Any = js.native
}

object DemoMain {

  val fileInput = document.getElementById("file_input").asInstanceOf[HTMLInputElement]

  val output = document.getElementById("output")

  def renderAttributes(attributes: List[AttributeModel]) = table(
    tbody(
      attributes.map(renderAttribute)
    )
  )

  def renderAttribute(attr: AttributeModel): TypedTag[Element] = {
    val extras =
      attr match {
        case c: AttributeModel.Code =>
          List(
            td("max stack: " + c.maxStack),
            td("max locals: " + c.maxLocals),
            td("instruction count: " + c.code.length),
            td("exception table count: " + c.exceptionTable.length),
            td("attributes: ", renderAttributes(c.attributes)),
          )

        case AttributeModel.Deprecated       => List(td("yes"))
        case AttributeModel.Scala            => List(td("yes"))
        case AttributeModel.Signature(value) => List(td(value))
        case AttributeModel.SourceFile(name) => List(td(name))
        case AttributeModel.TASTY(key)       => List(td(key.toString))

        case AttributeModel.Unsupported(name, bytes) =>
          td(s"attribute unknown: 0x${bytes.toHex}") :: Nil

        case AttributeModel.LineNumberTable(entries) =>
          td(
            table(
              thead(tr(th("instruction start"), th("line"))),
              tbody(
                entries.map { e =>
                  tr(td(e.startPc), td(e.lineNumber))
                }
              ),
            )
          ) :: Nil
        // case a => td("attribute unsupported") :: td(a.toString()) :: Nil
      }

    tr(td(attr.attrName), extras)
  }

  def renderMethod(method: MethodModel): TypedTag[Element] = tr(
    td(method.name),
    td(method.descriptor),
    td(
      renderAttributes(method.attributes)
    ),
  )

  def parseBytes(bytes: Array[Byte]): (ClassFile, ClassModel) = {
    output.replaceChildren(
      p(s"Read ", b(bytes.length), " bytes").render
    )

    val bits = ByteVector(bytes).bits

    val decodedFullE =
      ClassFileCodecs
        .classFile
        .decode(bits)
        .map(_.value)
        .toTry

    decodedFullE match
      case Failure(e) =>
        output.appendChild(
          p(style := "color: red", "Failed to decode file (see console for stack trace)").render
        )
        throw e
      case _ =>

    val decodedFull = decodedFullE.get

    val decoded = ExampleCode.decode(decodedFull)

    (decodedFull, decoded)
  }

  def handleBytes(
    bytes: Array[Byte]
  ): Unit = {
    val (decodedFull, decoded) = parseBytes(bytes)

    def renderCode(v: Any) = pre(
      code(
        `class` := "language-scala",
        pprint
          .PPrinter
          .apply(
            additionalHandlers = { case b: ByteVector => pprint.Tree.Literal(s"\"${b.toHex}\"") }
          )
          .apply(v)
          .plainText,
      )
    )

    output.appendChild(
      div(
        p("Loaded class: " + decoded.thisClass),
        p("Superclass: " + decoded.superClass),
        p("Constant pool size (allocated): " + decodedFull.constants.declaredSize),
        p("Attributes:", renderAttributes(decoded.attributes)),
        p("Methods:"),
        table(
          thead(
            tr(
              th("name"),
              th("descriptor"),
              th("attributes"),
            )
          ),
          tbody(
            decoded.methods.map(renderMethod)
          ),
        ),
        p("toString simple: "),
        renderCode(decoded),
        p("toString full: "),
        renderCode(decodedFull),
      ).render
    )

    Prism.highlightAll()
  }

  private def loadFile(cb: Array[Byte] => Unit) = {
    val file = fileInput.files.item(0)
    file
      .arrayBuffer()
      .`then`(bytes => cb(new Uint8Array(bytes).toArray.map(_.toByte)))
  }

  @JSExportTopLevel("fileUploaded")
  def fileUploaded(): Unit = loadFile(handleBytes)

  @JSExportTopLevel("fileUploadedSimple")
  def fileUploadedSimple(): Unit = loadFile { bytes =>
    val (decodedFull, decoded) = parseBytes(bytes)

    output.appendChild(
      div(
        h2("Class " + decoded.thisClass),
        p("Attributes:"),
        renderAttributes(decoded.attributes),
        p("Methods:"),
        table(
          thead(
            tr(th("name"), th("args"), th("instruction count"))
          ),
          tbody(
            decoded.methods.map { meth =>
              tr(
                td(meth.name),
                td(meth.descriptor),
                td(
                  meth
                    .attributes
                    .collectFirst { case c: AttributeModel.Code => c }
                    .fold(0)(_.code.size)
                ),
              )
            }
          ),
        ),
      ).render
    )
  }

}
