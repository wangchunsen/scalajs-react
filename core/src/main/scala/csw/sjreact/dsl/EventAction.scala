package csw.sjreact.dsl

import csw.sjreact.{EventChannel, StateMod}
import org.scalajs.dom.raw.{HTMLInputElement, KeyboardEvent}

trait EventAction[+T] {
  def apply(event: Event): T

  def map[R](fun: T => R): EventAction[R] = e => fun(this.apply(e))

  def flatMap[R](actionOp: T => EventAction[R]): EventAction[R] = e => {
    val value: T = this.apply(e)
    actionOp(value) apply e
  }

  /**
    * Run this action and then the next action and return the second result action as the result
    */
  def then[T](nextAction:EventAction[T]):EventAction[T] = e => {
    this.apply(e)
    nextAction.apply(e)
  }
}

object EventAction {
  val empty: EventAction[Unit] = _ => ()

  def inputValue: EventAction[String] = e => e.target.asInstanceOf[HTMLInputElement].value

  def keyCode: EventAction[Int] = e => e.asInstanceOf[KeyboardEvent].keyCode

  def ~>(mode: StateMod[String]): EventAction[Unit] = inputValue map mode.apply

  def const[T](action: => T): EventAction[T] = _ => action

  implicit class BindAble[T](val value: T) extends AnyVal {
    def ~>(mode: StateMod[T]): EventAction[Unit] = _ => mode(value)
  }

  def when(cond: Boolean)(action: => EventAction[_]): EventAction[Unit] =
    e => if (cond) action.apply(e)

  def fire[E](event:E)(implicit slot:EventChannel[E]):EventAction[Unit] = const(slot.fireEvent(event))
}
