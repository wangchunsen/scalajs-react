package csw.sjreact.dsl
import VDom._
trait TagDsl {
  val aDsl: AttributeDsl

  val label = Element("label")
  val div: Element = Element("div")
  val ul: Element = Element("ul")
  val li: Element = Element("li")
  val span: Element = Element("span")
  val a: Element = Element("a")
  val input: VoidElement = VoidElement("input")

  val br: VoidElement = VoidElement("br")
  val b:Element = Element("b")

  val form:Element = Element("form")

  object inputs {
    val text: VoidElement = input(aDsl.`type` := "text")
    val password: VoidElement = input(aDsl.`type` := "password")
  }

  val button: Element = Element("button")

  val h4: Element = Element("h4")
}