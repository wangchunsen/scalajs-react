package csw

import csw.sjreact.dsl.VDom
import csw.sjreact.react.View

import scala.language.implicitConversions

package object sjreact {
  type StateMod[T] = T => Unit

  def state[T](value: T): State[T] = Var.state(value)

  def view(fun: WatchScope => VDom.Element): View = {
    new View {
      override def render: VDom.Element = fun(this.manager)
    }
  }
}
