package csw.sjreact.util


trait WatchAble[T] {
  type Watcher = T => Any
  private var watchers: Seq[Watcher] = Nil

  def addWatcher(watcher: Watcher): CancelAble = {
    watchers = watchers :+ watcher
    () => removeWatcher(watcher)
  }

  def notifyAllWatchers(value: T): Unit = watchers.foreach { watcher =>
    try {
      watcher(value)
    } catch {
      case e: Exception => println(e)
    }
  }

  def notifyOne(watcher: Watcher, value: T): Unit = {
    try {
      watcher(value)
    } catch {
      case e: Exception => println(e)
    }
  }

  private def removeWatcher(watcher: Watcher): Unit = watchers = watchers.filterNot(watcher eq _)
}
