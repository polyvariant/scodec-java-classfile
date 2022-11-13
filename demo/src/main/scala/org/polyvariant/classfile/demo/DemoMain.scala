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

object DemoMain {

  val fileInput = document.getElementById("file_input").asInstanceOf[HTMLInputElement]

  val output = document.getElementById("output")

  def renderMethod(method: MethodModel): TypedTag[Element] = tr(
    td(method.name),
    td(method.descriptor),
    td(
      table(
        tbody(
          method.attributes.map { attr =>
            val extras =
              attr match {
                case c: AttributeModel.Code =>
                  List(
                    td("max stack: " + c.maxStack),
                    td("max locals: " + c.maxLocals),
                    td("instruction count: " + c.code.length),
                    td("exception table count: " + c.exceptionTable.length),
                    td("attribute count: " + c.attributes.length),
                  )
                case _ => td("attribute unknown") :: Nil
              }

            tr(
              td(attr.attrName),
              extras,
            )
          }
        )
      )
    ),
  )

  def handleBytes(
    bytes: Array[Byte]
  ): Unit = {
    output.replaceChildren(
      p(s"Loaded ", b(bytes.length), " bytes").render
    )

    val bits = ByteVector(bytes).bits

    val decodedE =
      ClassFileCodecs
        .classFile
        .decode(bits)
        .map(_.value)
        .map(ExampleCode.decode)
        .toTry

    decodedE match
      case Failure(e) =>
        output.appendChild(
          p(style := "color: red", "Failed to decode file (see console for stack trace)").render
        )
        e.printStackTrace()
        return
      case _ =>

    val decoded = decodedE.get

    output.appendChild(
      div(
        p("Loaded class: " + decoded.thisClass),
        p("Superclass: " + decoded.superClass),
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
        p("toString: "),
        pre(
          code(
            pprint
              .PPrinter
              .apply(
                additionalHandlers = { case b: ByteVector =>
                  pprint.Tree.Literal(s"\"${b.toHex}\"")
                }
              )
              .apply(decoded)
              .plainText
          )
        ),
      ).render
    )
  }

  def main(args: Array[String]): Unit =
    fileInput.onchange = { _ =>
      val file = fileInput.files.item(0)
      file
        .arrayBuffer()
        .`then`(bytes => handleBytes(new Uint8Array(bytes).toArray.map(_.toByte)))
    }

}
