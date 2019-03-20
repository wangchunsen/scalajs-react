package csw.sjreact.util


trait Watcher[T] {
  private val identity = GlobalId.nextId

  def notified(value: T): Any

  override def toString: String = "watcher " + identity
}

object Watcher {

  trait CancelAware {
    def canceled(): Unit
  }


  def ignoreInitialNotify[T](watcher: Watcher[T]): Watcher[T] = {
    var isFirstTime = true
    value => {
      if (!isFirstTime) watcher.notified(value)
      else isFirstTime = false
    }
  }

  def chancelAware[T](watcher: Watcher[T], callback: () => Any): Watcher[T] = new Watcher[T] with CancelAware {
    override def canceled(): Unit = callback.apply()

    override def notified(value: T): Any = watcher.notified(value)
  }
}
