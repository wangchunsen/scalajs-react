package csw.sjreact.dsl

import csw.sjreact.dsl.VDom.{ActionAttribute, Attribute, EmptyAttribute, ValueAttribute}

case class AttrKey[T](attrName: String) {
  def :=(value: T): Attribute = value match {
    case a: EventAction[_] => ActionAttribute(attrName, a.apply)
    case _ => ValueAttribute(attrName, value)
  }

  def :=? (optValue: Option[T]):Attribute =  optValue match {
    case None => EmptyAttribute
    case Some(value) => this.:=(value)
  }
}

object AttrKey {
  def action(name: String): AttrKey[EventAction[_]] = AttrKey(name)

  def attr(name: String): AttrKey[String] = AttrKey(name)

  def bool(name: String): AttrKey[Boolean] = AttrKey(name)
}