package csw.sjreact.util

import csw.sjreact.util.Watcher.CancelAware


trait WatchAble[T] {
  private var watchers: Seq[Watcher[T]] = Seq.empty

  def addWatcher(watcher: Watcher[T]): CancelAble = {
    watchers = watchers :+ watcher
    () => removeWatcher(watcher)
  }

  def notifyAllWatchers(value: T): Unit = {
    watchers.foreach { watcher =>
      try {
        watcher.notified(value)
      } catch {
        case e: Exception => println(e)
      }
    }
  }

  def notifyOne(watcher: Watcher[T], value: T): Unit = {
    try {
      watcher.notified(value)
    } catch {
      case e: Exception => println(e)
    }
  }

  private def removeWatcher(watcher: Watcher[T]): Unit = {
    watchers = watchers.filterNot(_ eq watcher)

    watcher match {
      case a: CancelAware => a.canceled()
      case _ =>
    }
  }
}
