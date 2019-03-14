package csw.sjreact.react

import csw.sjreact.util.RawJs

import scala.scalajs.js

private[react] class Wrapper[+T] extends js.Any {
  def value: T = js.native

}

private[react] object Wrapper {
  val empty: Wrapper[Nothing] = apply(()).asInstanceOf[Wrapper[Nothing]]

  def isEmpty(wrapper: Wrapper[_]): Boolean =
    (empty eq wrapper) || RawJs.isEmpty(wrapper) || RawJs.isEmpty(wrapper.value)

  def apply[T](value: T): Wrapper[T] =
    if (RawJs.isEmpty(value)) empty
    else
      js.Dynamic.literal(
        "value" -> value.asInstanceOf[js.Any]
      ).asInstanceOf[Wrapper[T]]

  def unWrap[T](wrapper: Wrapper[T]): Option[T] =
    if (isEmpty(wrapper)) None
    else Some(wrapper.value)


  def eq(w1: Wrapper[_], w2: Wrapper[_]): Boolean = (w1 eq w2) || unWrap(w1) == unWrap(w2)
}