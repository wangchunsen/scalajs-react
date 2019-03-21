package csw.sjreact

import csw.sjreact.implicits.csw.sjreact.Combiner
import csw.sjreact.util.OptionOps.when
import csw.sjreact.util._

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

sealed trait Var[T] {
  def value: T
}

trait State[T] extends Var[T] {
  def set(value: T): Unit

  def update(fun: T => T)

  def updateOpt(fun: T => Option[T])
}

object State {

  implicit class StateOps[S](val state: State[S]) extends AnyVal {

    def updateItemWhere[A <: AnyRef]
    (pred: A => Boolean, fun: A => A)
    (implicit is: S <:< SeqLike[A, S], bf: CanBuildFrom[S, A, S]) {

      state.updateOpt { s =>
        val seq = is(s)
        val i = seq.indexWhere(pred)
        when(i >= 0) {
          val originItem = seq(i)
          seq.updated(i, fun(originItem))
        }
      }
    }

    def updateItem[A <: AnyRef](item: A, fun: A => A)(implicit is: S <:< SeqLike[A, S], bf: CanBuildFrom[S, A, S]): Unit =
      updateItemWhere[A](_ eq item, fun)

    def removeItem[A <: AnyRef](item: A)(implicit is: S <:< SeqLike[A, S], bf: CanBuildFrom[S, A, S]): Unit =
      state.update(_.filterNot(_ eq item))

    def :=(value: S): S = {
      state.set(value)
      value
    }
  }

}

object Var {

  private trait PrivateVar[T] extends Var[T] {
    /*
      Add a watcher to this var
      Must make sure the watcher is notified for the first time attach
      */
    def addWatcher(watcher: Watcher[T])(implicit manager: WatchScope): CancelAble
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

    override def addWatcher(watcher: Watcher[T])(implicit manager: WatchScope): CancelAble = {
      val cancelAble = manager.addWatcher(this, watcher)
      //make sure the watcher is notified after attach
      notifyOne(watcher, value)
      cancelAble
    }
  }

  private case class VarImp[T, R](tracked: Var[T], mapper: T => R) extends PrivateVar[R] {
    def addWatcher(watcher: Watcher[R])(implicit manager: WatchScope): CancelAble = {
      //make sure the watcher is notified for first time
      var previous: Option[R] = None
      tracked.asInstanceOf[PrivateVar[T]] addWatcher (changeValue => {
        val newValue = mapper(changeValue)
        if (!previous.contains(newValue)) {
          previous = Some(newValue)
          watcher.notified(newValue)
        }
      })
    }

    override def value: R = mapper(tracked.value)
  }

  private class ZippedVar[T, V, R](v1: Var[T], v2: Var[V])(implicit combiner: Combiner[T, V, R])
    extends PrivateVar[R] {
    private val varId = GlobalId.nextId

    override def value: R = combiner(v1.value, v2.value)

    def addWatcher(watcher: Watcher[R])(implicit manager: WatchScope): CancelAble = {
      val watcherId = GlobalId.nextId

      def changed(v1: T, v2: V) {
        watcher.notified(combiner(v1, v2))
      }

      //stop the initial fire, we will do it manually
      //cause this is tracked from multiple var, we need to stop it fires multiple time for intial
      val w1 = v1.asInstanceOf[PrivateVar[T]] addWatcher Watcher.ignoreInitialNotify[T] { value =>
        changed(value, v2.value)
      }

      val w2 = v2.asInstanceOf[PrivateVar[V]] addWatcher Watcher.ignoreInitialNotify[V] { value =>
        changed(v1.value, value)
      }

      try {
        //do the initial fire
        watcher.notified(value)
      } catch {
        case e: Exception => println(e)
      }

      () => {
        w1.cancel()
        w2.cancel()
      }
    }
  }

  private class SeqVar[T](vars: Seq[Var[T]]) extends PrivateVar[Seq[T]] {
    override def value: Seq[T] = vars.map(_.value)

    override def addWatcher(watcher: Watcher[Seq[T]])(implicit manager: WatchScope): CancelAble = {
      var cached: IndexedSeq[T] = value.toIndexedSeq

      def notify(value: Seq[T]) = try {
        watcher.notified(cached)
      } catch {
        case e: Exception => println(e)
      }

      def updated(value: T, index: Int) = {
        cached = cached.updated(index, value)
        notify(cached)
      }

      val cancelAbles = vars.zipWithIndex.map { t2 =>
        val (var_, index) = t2
        //stop the initial fire, we will do it manually
        //cause this is tracked from multiple var, we need to stop it fires multiple time for intial
        var_.asInstanceOf[PrivateVar[T]] addWatcher Watcher.ignoreInitialNotify[T] { value =>
          updated(value, index)
        }
      }

      //initial notify
      notify(value)

      () => cancelAbles.foreach(_.cancel())
    }
  }

  /*
    Listening changes of o var
    Specially, the watcher will be noticed immediately after attach
   */
  def watch[T](var_ : Var[T], watcher: Watcher[T])(implicit manager: WatchScope): CancelAble =
    var_.asInstanceOf[PrivateVar[T]].addWatcher(watcher)

  implicit class VarOps[T](val self: Var[T]) extends AnyVal {
    def map[R](mapper: T => R)(implicit watchManager: WatchScope): Var[R] = VarImp(self, mapper)

    def onChange(watcher: Watcher[T])(implicit manager: WatchScope): CancelAble = Var.watch(self, watcher)

    def ~[V, R](var_ : Var[V])(implicit combiner: Combiner[T, V, R]): Var[R] = new ZippedVar(self, var_)

  }

  def seq[T](vars: Var[T]*): Var[Seq[T]] = new SeqVar[T](vars)

  def state[T](initValue: T): State[T] = new StateImp[T](initValue)
}