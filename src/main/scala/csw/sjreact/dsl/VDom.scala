package csw.sjreact.dsl

import csw.sjreact.react.RenderAble

import scala.language.implicitConversions

sealed trait VDom

object VDom {
  implicit def textNode(text: String): Node = Text(text)

  sealed trait Node extends VDom

  sealed trait Attribute extends VDom

  case class Text(text: String) extends Node

  case class ValueAttribute(key: String, value: Any) extends Attribute

  case class ActionAttribute(key: String, value: Event => Any) extends Attribute

  object EmptyAttribute extends Attribute

  case class VoidElement(tagName: String, attributes:Seq[Attribute] = Seq.empty) extends Node {
    def apply(attributes: Attribute*): VoidElement = {
      this.copy(attributes = this.attributes ++ attributes)
    }
  }

  case class RenderAbleNode(component:RenderAble) extends Node

  case class Element(tagName: String,
                     attributes:Seq[Attribute] = Seq.empty,
                     children: Seq[Node] = Seq.empty) extends Node {

    def apply(items: VDom*): Element = {
      val (attributes: Seq[Attribute], children: Seq[Node]) = items.partition(_.isInstanceOf[Attribute])
      copy(
        attributes = this.attributes ++ attributes,
        children = this.children ++ children)
    }
  }
}