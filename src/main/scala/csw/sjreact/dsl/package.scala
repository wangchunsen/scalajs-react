package csw.sjreact

import csw.sjreact.dsl.VDom.RenderAbleNode
import csw.sjreact.react.{Comp, RenderAble}

import scala.language.implicitConversions


package object dsl {

  object ^ extends AttributeDsl

  object < extends TagDsl {
    override val aDsl: AttributeDsl = ^
  }

  type Event = org.scalajs.dom.raw.Event

  implicit def node2Element(node: VDom.Node): RenderAble = Comp.static(node)

  implicit def renderAbleNode(component: RenderAble): RenderAbleNode = RenderAbleNode(component)

  implicit def toOption[T](value: T): Option[T] = Option(value)

}
