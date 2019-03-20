package csw.sjreact

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait EventChannel[-E] {

  def processor: PartialFunction[E, Any]

  private def processEvent[E1 <: E](e: E1): Unit = {
    processor.applyOrElse[E1, Any](e, e => println(e))
  }

  def fireEvent[E1 <: E](event: E1, async: Boolean = true): Unit =
    if (async) Future {
      processEvent(event)
    }
    else processEvent(event)
}

object EventChannel {
  def apply[E](_processor: PartialFunction[E, Any]) = new EventChannel[E] {
    override def processor: PartialFunction[E, Any] = processor
  }
}