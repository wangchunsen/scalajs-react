package csw.sjreact.react

import scala.language.implicitConversions

trait CompName {
  def nameStr: Option[String]
}

object CompName {
  val empty: CompName = new CompName {
    override def nameStr: Option[String] = None
  }

  implicit def fromStr(name: String) = new CompName {
    override def nameStr: Option[String] = Some(name)
  }
}

