package csw.sjreact.util

object Watchers {
  type Watcher[T] = T => Any

  def ignoreInitialNotify[T](watcher: Watcher[T]): Watcher[T] = {
    var isFirstTime = true
    value => {
      if (!isFirstTime) watcher(value)
      else isFirstTime = false
    }
  }
}
