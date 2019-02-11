package csw.sjreact

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom


object StateOps {

  implicit class StateMode[T](val state: State[T]) extends AnyVal {
    def update(fun: T => T): Unit = state.set(fun(state.get))
  }

  implicit class StateSeqMode[A <: AnyRef, Col[+T] <: SeqLike[T,Col[T]]](val state: State[Col[A]]) extends AnyVal {

    def updateItemWhere(pred: A => Boolean, fun: A => A)
                       (implicit bf: CanBuildFrom[Col[A], A, Col[A]]) {

      val seq = state.get
      val i = seq.indexWhere(pred)
      if (i >= 0) {
        val originItem = seq(i)
        val col:Col[A] = seq.updated(i, fun(originItem))
        state.set(col)
      }
    }

    def updateItem(item: A, fun: A => A)(implicit bf: CanBuildFrom[Col[A], A, Col[A]])
    = updateItemWhere(_ eq item, fun)

    def removeItem(item: A) =
      state.set(state.get.filterNot(_ eq item))
  }

}
