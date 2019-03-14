package csw.sjreact

import csw.sjreact.util.{CancelAble, WatchAble}

import scala.collection.mutable.ArrayBuffer

trait WatchScope {
  implicit protected val wm: WatchScope = this

  private val listeners: ArrayBuffer[CancelAble] = ArrayBuffer.empty

  def addWatcher[T](target: WatchAble[T])(watcher: target.Watcher): CancelAble = {
    val cancelAble = target.addWatcher(watcher)

    listeners append cancelAble

    () => {
      cancelAble.cancel()
      listeners -= cancelAble
    }
  }

  def cancelAllWatching(): Unit = {
    listeners foreach {
      _.cancel()
    }
    listeners.clear()
  }
}
