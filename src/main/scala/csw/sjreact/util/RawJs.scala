package csw.sjreact.util

import scala.scalajs.js
import scala.scalajs.js.UndefOr

object RawJs {
  def assign(obj: js.Object, key: String, value: js.Any): Unit = {
    val dynamic = obj.asInstanceOf[js.Dynamic]
    dynamic.updateDynamic(key)(value)
  }

  def get[T <: js.Any](obj: js.Object, key: String): UndefOr[T] = {
    obj.asInstanceOf[js.Dynamic].selectDynamic(key).asInstanceOf[T]
  }

  def isEmpty(any: Any): Boolean = (any == null) || js.isUndefined(any)
}
