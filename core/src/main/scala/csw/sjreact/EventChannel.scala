package csw.sjreact

import scala.collection.mutable.ArrayBuffer


class EventChannel[-E](processor: PartialFunction[E, Any]) {

  import scala.scalajs.js.timers._

  private[this] val events = ArrayBuffer.empty[E]

  private def processEvent[E1 <: E](e: E1): Unit = {
    processor.applyOrElse[E1, Any](e, e => println(e))
  }

  def fireEvent[E1 <: E](event: E1, async: Boolean = true): Unit =
    if (async) {
      //if this event queue is not empty, there must be a pending timeout callback
      if (events.isEmpty) {
        setTimeout(0) {
          val eventsQueue = this.events.toList
          this.events.clear()
          eventsQueue.foreach(processEvent)
        }
      }
      this.events.append(event)
    } else processEvent(event)
}