package csw

import csw.sjreact.dsl.VDom
import csw.sjreact.react.View

import scala.scalajs.js

package object sjreact {
  type StateMod[T] = T => Unit

  def state[T](value: T): State[T] = Var.state(value)

  def view(fun: WatchScope => VDom.Element): View = {
    new View {
      override def render: VDom.Element = fun(this.manager)
    }
  }

  implicit def mapToDictionary(map: Map[String, String]): js.Dictionary[String] = js.Dictionary(map.toSeq: _*)
}
