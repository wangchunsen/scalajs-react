package csw.sjreact

import csw.sjreact.util.{CacheAbleComputer, CancelAble, WatchAble}

sealed trait Data[T] {
  def get: T
}

trait State[T] extends Data[T] {
  def set(value: T): Unit
}

object Data {

  private trait VarPeer[T] {
    self: WatchAble[T] =>
    def onChanged(value: T): Unit = {
      self.notifyWatchers(value)
    }
  }

  private trait Wrapper[T] extends Data[T] {
    val state: State[T]

    def get = state.get
  }

  def watch[T](_var: Data[T])(watcher: T => Any): CancelAble = _var match {
    case a: WatchAble[T] => a.addWatcher(watcher)
    case wrapper: Wrapper[T] => watch(wrapper.state)(watcher)
    case any => throw new Exception(s"Cant watch the state $any")
  }

  def toData[T](state: State[T]): Data[T] = {
    val stateToWrap = state
    new Wrapper[T] {
      override val state: State[T] = stateToWrap
    }
  }

  def state[T](initValue: T): State[T] = new State[T] with VarPeer[T] with WatchAble[T] {
    self =>
    private var value = initValue

    override def set(newValue: T): Unit = {
      val current = self.value
      if (newValue != current) {
        self.value = newValue
        onChanged(value)
      }
    }

    override def get: T = value
  }

  def map[T, R](target: Data[T])(mapper: T => R): Data[R] =
    new Data[R] with VarPeer[R] with WatchAble[R] {
      val cacheAbleMapper = new CacheAbleComputer[T, R](mapper)

      override def get: R = cacheAbleMapper.cachedOrCompute(target.get)

      watch(target)(value => {
        val previousCache = cacheAbleMapper.cached
        val newValue = cacheAbleMapper.compute(value)
        if (!previousCache.contains(newValue)) {
          onChanged(newValue)
        }
      })
    }

  def zip[T, V](state: Data[T], state1: Data[V]): Data[(T, V)] =
    zipSeqBy(Vector(state, state1), vec => (vec(0).asInstanceOf[T], vec(1).asInstanceOf[V]))

  def zipBy[T,V, R](state: Data[T], state1: Data[V])(fun:(T,V) => R):Data[R] =
    zipSeqBy(Vector(state,state1), vec => fun(vec(0).asInstanceOf[T], vec(1).asInstanceOf[V]))

  private def zipSeqBy[T](seq: Vector[Data[_]], tran: Vector[Any] => T): Data[T] = {
    new Data[T] with VarPeer[T] with WatchAble[T] {
      self =>

      private var valueVec: Vector[Any] = seq.map(_.get)
      private var value = tran(valueVec)

      override def get: T = value

      private val watchers = seq.zipWithIndex.map(t2 => {
        val (state, index) = t2
        watch(state)(v => {
          self.valueVec = self.valueVec.updated(index, v)
          val previousValue = value
          val newValue = tran(self.valueVec)
          if(newValue != previousValue){
            value = newValue
            notifyWatchers(value)
          }
        })
      })
    }

  }
}