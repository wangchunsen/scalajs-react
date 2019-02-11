package demo

import csw.sjreact.dsl.EventAction._
import csw.sjreact.dsl.VDom._
import csw.sjreact.dsl.{<, ^, _}
import csw.sjreact.react.Comp
import csw.sjreact.{Data, EventChannel, State, StateMod}
import org.scalajs.dom

object Filters extends Enumeration {
  type Filter = Value
  val All, Active, Completed = Value
}

case class TodoItem(content: String, complete: Boolean = false)


trait ToEvent

case class Add(content: String) extends ToEvent

case class Delete(item: TodoItem) extends ToEvent

case class ChangeState(item: TodoItem) extends ToEvent

object Main {
  def renderItem(todoItem: TodoItem)(implicit channel: EventChannel[ToEvent]): Node = {
    <.li(
      <.div(
        <.input(^.`class` := "toggle", ^.`type` := "checkbox", ^.checked := todoItem.complete,
          ^.onChange := fire(ChangeState(todoItem))),
        <.label(todoItem.content),
        <.button(^.`class` := "destroy", ^.onClick := fire(Delete(todoItem)))
      )
    )
  }

  def renderItems(items: Seq[TodoItem])(implicit channel: EventChannel[ToEvent]): Node = <.ul(^.`class` := "todo-list")(items.map(renderItem): _*)


  def renderFilter(current: Filters.Filter, $: StateMod[Filters.Filter]) =
    <.div(
      Filters.values.toSeq.map(filter => {
        val span = <.span(filter.toString, ^.onClick := filter ~> $)
        if (filter == current) <.b(span)
        else span
      }): _*)


  def renderInput(str: String, $: StateMod[String])(implicit channel: EventChannel[ToEvent]): Node =
    <.input(^.placeholder := "What needs to be done?", ^.value := str,
      ^.onChange := ~>($),
      ^.onKeyDown := keyCode flatMap { code =>
        when(code == 13)("" ~> $ then fire(Add(str)))
      })

  def main(args: Array[String]): Unit = {
    import csw.sjreact.StateOps._
    val allItems = Data.state(Seq.empty[TodoItem])

    val filter: State[Filters.Filter] = Data.state(Filters.All)

    val showItems = Data.zipBy(allItems, filter)((items, filter) => filter match {
      case Filters.All => items
      case Filters.Active => items.filterNot(_.complete)
      case _ => items.filter(_.complete)
    })

    val inputValue = Data.state("")

    implicit val channel = new EventChannel[ToEvent]({
      case Add(str) => allItems.set(allItems.get :+ TodoItem(str))
      case ChangeState(item) =>
        allItems.updateItem(item, i => i.copy(complete = !i.complete))
      case Delete(item) => allItems.removeItem(item)
    })

    Comp.renderIntoDom(
      <.div(
        Comp.renderS(inputValue)(renderInput),
        Comp.render(showItems)(renderItems),
        Comp.renderS(filter)(renderFilter)
      ),
      dom.document.getElementById("app-content"))
  }

}
