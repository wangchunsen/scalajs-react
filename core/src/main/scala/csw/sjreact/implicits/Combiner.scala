package csw.sjreact.implicits

package csw.sjreact

import scala.language.implicitConversions

trait Combiner[-T, -V, +R] {
  def apply(t: T, v: V): R
}

trait LowerPriorityCombiner {
  implicit def toTuple[T, V]: Combiner[T, V, (T, V)] = (t: T, v: V) => t -> v
}

object Combiner extends LowerPriorityCombiner {

  implicit def t2left[T1, T2, R]: Combiner[(T1, T2), R, (T1, T2, R)] = (t, v) => (t._1, t._2, v)

  implicit def t3left[T1, T2, T3, R]: Combiner[(T1, T2, T3), R, (T1, T2, T3, R)] = (t, v) => (t._1, t._2, t._3, v)

  implicit def t4left[T1, T2, T3, T4, R]: Combiner[(T1, T2, T3, T4), R, (T1, T2, T3, T4, R)] = (t, v) => (t._1, t._2, t._3, t._4, v)

}
