package csw.sjreact

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import util.OptionOps._

import scala.language.higherKinds

trait SeqStateOps[A <: AnyRef, Col[+T] <: SeqLike[T, Col[T]]] extends Any {
  def state: State[Col[A]]

  def updateItemWhere(pred: A => Boolean, fun: A => A)
                     (implicit bf: CanBuildFrom[Col[A], A, Col[A]]) {

    state.updateOpt { seq =>
      val i = seq.indexWhere(pred)
      when(i >= 0) {
        val originItem = seq(i)
        seq.updated(i, fun(originItem))
      }
    }
  }

  def updateItem(item: A, fun: A => A)(implicit bf: CanBuildFrom[Col[A], A, Col[A]]) =
    updateItemWhere(_ eq item, fun)

  def removeItem(item: A) =
    state.update(_.filterNot(_ eq item))
}
