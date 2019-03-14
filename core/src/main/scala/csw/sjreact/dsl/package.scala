package csw.sjreact

import csw.sjreact.dsl.VDom.{EmptyNode, RenderAbleNode}
import csw.sjreact.react.{RenderAble, View}

import scala.language.implicitConversions


package object dsl {

  object ^ extends AttributeDsl

  object < extends TagDsl {
    override val aDsl: AttributeDsl = ^
  }

  type Event = org.scalajs.dom.raw.Event

  implicit def node2Element(node: VDom.Node): RenderAble = RenderAble.static(node)

  implicit def view2RenderAble(view: View): RenderAble = RenderAble.ofView(view)

  implicit def renderAbleNode(component: RenderAble): RenderAbleNode = RenderAbleNode(component)

  implicit def fromOption(opt: Option[VDom.Node]): VDom.Node = opt match {
    case Some(n) => n
    case _ => EmptyNode
  }
}
