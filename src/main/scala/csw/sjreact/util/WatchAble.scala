package csw.sjreact.util


trait WatchAble[T] {
  type Watcher = T => Any
  private var watchers: Seq[Watcher] = Nil

  def addWatcher(watcher: Watcher): CancelAble = {
    watchers = watchers :+ watcher
    () => removeWatcher(watcher)
  }

  def notifyWatchers(value: T): Unit = watchers.foreach { listener =>
    try {
      listener(value)
    } catch {
      case e: Exception => println(e)
    }
  }

  private def removeWatcher(watcher: Watcher): Unit = watchers = watchers.filterNot(watcher eq _)
}
