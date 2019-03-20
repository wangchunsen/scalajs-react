package csw.sjreact

import csw.sjreact.util.{CancelAble, WatchAble, Watcher}

import scala.collection.mutable.ArrayBuffer

trait WatchScope {
  implicit protected val wm: WatchScope = this

  private val listeners: ArrayBuffer[CancelAble] = ArrayBuffer.empty

  def addWatcher[T](target: WatchAble[T], watcher: Watcher[T]): CancelAble = {
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
