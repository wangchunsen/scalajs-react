package csw.sjreact.util

import scala.language.implicitConversions

class Or[A, B] private(private val obj: Any) extends AnyVal {
  def value: Any = obj

  def asLeft: A = obj.asInstanceOf[A]

  def asRight: B = obj.asInstanceOf[B]
}

object Or {
  type |[A, B] = Or[A, B]

  implicit def ofLeft[A, B](value: A): A | B = new Or(value)

  implicit def ofRight[A, B](value: B): A | B = new Or(value)
}
