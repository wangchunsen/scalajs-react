package csw.sjreact

import csw.sjreact.implicits.csw.sjreact.Combiner
import csw.sjreact.util.{CancelAble, WatchAble, Watchers}

import scala.collection.SeqLike
import scala.language.higherKinds

sealed trait Var[T] {
}

trait State[T] extends Var[T] {
  def set(value: T): Unit

  def update(fun: T => T)

  def updateOpt(fun: T => Option[T])
}

object State {

  implicit class SeqOps[A <: AnyRef, Col[+T] <: SeqLike[T, Col[T]]](val state: State[Col[A]])
    extends AnyVal with SeqStateOps[A, Col]
}

object Var {
  def unapply[T](arg: Var[T]): Option[T] = Some(arg.asInstanceOf[PrivateVar[T]].value)

  private trait PrivateVar[T] extends Var[T] {
    def value: T

    /*
      Add a watcher to this var
      Must make sure the watcher is notified for the first time attach
      */
    def addWatcher(watcher: T => Any)(implicit manager: WatchScope): CancelAble
  }

  private class StateImp[T](var value: T)
    extends State[T] with WatchAble[T] with PrivateVar[T] {
    override def set(newValue: T): Unit = {
      val current = this.value
      if (newValue != current) {
        this.value = newValue
        notifyAllWatchers(value)
      }
    }

    override def update(fun: T => T): Unit = set(fun(value))

    override def updateOpt(fun: T => Option[T]): Unit = fun(value) map set

    override def addWatcher(watcher: T => Any)(implicit manager: WatchScope): CancelAble = {
      val cancelAble = manager.addWatcher(this)(watcher)
      //make sure the watcher is notified after attach
      notifyOne(watcher, value)
      cancelAble
    }
  }

  private case class VarImp[T, R](tracked: Var[T], mapper: T => R) extends PrivateVar[R] {
    def addWatcher(watcher: R => Any)(implicit manager: WatchScope): CancelAble = {
      //make sure the watcher is notified for first time
      var previous: Option[R] = None
      tracked.asInstanceOf[PrivateVar[T]] addWatcher (changeValue => {
        val newValue = mapper(changeValue)
        if (!previous.contains(newValue)) {
          previous = Some(newValue)
          watcher(newValue)
        }
      })
    }

    override def value: R = unapply(tracked).map(mapper).get
  }

  private class ZippedVar[T, V, R](v1: Var[T], v2: Var[V])(implicit combiner: Combiner[T, V, R])
    extends PrivateVar[R] {
    override def value: R = combiner(unapply(v1).get, unapply(v2).get)

    def addWatcher(watcher: R => Any)(implicit manager: WatchScope): CancelAble = {

      def changed(v1: T, v2: V) {
        watcher(combiner(v1, v2))
      }

      //stop the initial fire, we will do it manually
      //cause this is tracked from multiple var, we need to stop it fires multiple time for intial
      val w1 = v1.asInstanceOf[PrivateVar[T]] addWatcher Watchers.ignoreInitialNotify[T] { value =>
        changed(value, unapply(v2).get)
      }
      val w2 = v2.asInstanceOf[PrivateVar[V]] addWatcher Watchers.ignoreInitialNotify[V] { value =>
        changed(unapply(v1).get, value)
      }

      try {
        //do the initial fire
        val value = combiner(unapply(v1).get, unapply(v2).get)
        watcher(value)
      } catch {
        case e: Exception => println(e)
      }

      () => {
        w1.cancel()
        w2.cancel()
      }
    }
  }

  /*
    Listening changes of o var
    Specially, the watcher will be noticed immediately after attach
   */
  def onChange[T](var_ : Var[T], watcher: T => Any)(implicit manager: WatchScope): CancelAble =
    var_.asInstanceOf[PrivateVar[T]].addWatcher(watcher)

  implicit class VarOps[T](val self: Var[T]) extends AnyVal {
    def map[R](mapper: T => R)(implicit watchManager: WatchScope): Var[R] = VarImp(self, mapper)

    def onChange(watcher: T => Any)(implicit manager: WatchScope): CancelAble = Var.onChange(self, watcher)

    def ~[V, R](var_ : Var[V])(implicit combiner: Combiner[T, V, R]): Var[R] = new ZippedVar(self, var_)
  }


  def state[T](initValue: T): State[T] = new StateImp[T](initValue)
}