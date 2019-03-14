# scalajs-react
A easy to use library that build apps by scalajs on top of react.js

# Todo mvc example
```Scala
package demo

import csw.sjreact._
import csw.sjreact.dsl.EventAction._
import csw.sjreact.dsl.VDom._
import csw.sjreact.dsl.{<, ^, _}
import csw.sjreact.react.{Comp, View}
import csw.sjreact.util.OptionOps
import csw.sjreact.{EventChannel, State, StateMod, Var}
import org.scalajs.dom

object Filters extends Enumeration {
  type Filter = Value
  val All, Active, Completed = Value
}

case class TodoItem(content: String, complete: Boolean = false)


trait ToDoEvent

case class Add(content: String) extends ToDoEvent

case class Delete(item: TodoItem) extends ToDoEvent

case class ChangeState(item: TodoItem) extends ToDoEvent

object ClearComplete extends ToDoEvent
object ToggleAll extends ToDoEvent

object Main {
  def renderItem(todoItem: TodoItem)(implicit channel: EventChannel[ToDoEvent]): Node = {
    <.li(
      <.div(
        <.input(^.`class` := "toggle", ^.`type` := "checkbox", ^.checked := todoItem.complete,
          ^.onChange := fire(ChangeState(todoItem))),
        <.label(todoItem.content),
        <.button(^.`class` := "destroy", ^.onClick := fire(Delete(todoItem)))
      )
    )
  }

  def renderItems(items: Seq[TodoItem])(implicit channel: EventChannel[ToDoEvent]): Node =
    <.ul(^.`class` := "todo-list")(items.map(renderItem): _*)


  def renderFilter(current: Filters.Filter, $: StateMod[Filters.Filter]) =
    <.div(
      Filters.values.toSeq.map(filter => {
        val span = <.span(filter.toString, ^.onClick := filter ~> $, ^.style := Map("paddingRight" -> "5px"))
        if (filter == current) <.b(span)
        else span
      }): _*)

  def renderInput(str: String, $: StateMod[String])(implicit channel: EventChannel[ToDoEvent]): Node =
    <.input(^.placeholder := "What needs to be done?", ^.value := str,
      ^.onChange := ~>($),
      ^.onKeyDown := keyCode flatMap { code =>
        when(code == 13)("" ~> $ then fire(Add(str)))
      })

  def main(args: Array[String]): Unit = {
    val rootView = view { implicit manager =>
      val allItems = Var.state(Seq.empty[TodoItem])
      val complelted = allItems map { seq =>
        seq.filter(_.complete)
      }
      val actived = allItems map (_.filterNot(_.complete))

      val filter: State[Filters.Filter] = Var.state(Filters.All)

      val inputValue = Var.state("")

      implicit val channel = new EventChannel[ToDoEvent]({
        case Add(str) => allItems.update(_ :+ TodoItem(str))
        case ChangeState(item) =>
          allItems.updateItem(item, i => i.copy(complete = !i.complete))
        case Delete(item) => allItems.removeItem(item)
        case ClearComplete => allItems.update(_.filterNot(_.complete))
        case ToggleAll => allItems.update{ items =>
          val hasUncomplete = items.exists(!_.complete)

          if(hasUncomplete) items.map(_.copy(complete = true))
          else items.map(_.copy(complete = false))
        }
      })

      val remainItems = actived map (_.size)

      <.div(
        <.div(^.id := "header",
          Comp.render(allItems)(items =>
            <.button(^.`class` :=? OptionOps.when(items.forall(_.complete))("active")  , "X", ^.onClick := fire(ToggleAll))
          ),
          Comp.renderS(inputValue, "Todo-input")(renderInput)
        ),
        Comp.render(filter) {
          case Filters.All => Comp.render(allItems, "All-items")(renderItems)
          case Filters.Active => Comp.render(actived, "Active-items")(renderItems)
          case _ => Comp.render(complelted, "Complete-items")(renderItems)
        },
        <.div(^.id := "footer",
          Comp.render(remainItems, "Counter")(count =>
            <.div(^.id := "counter",
              if (count > 1) s"$count items left" else s"$count item left"
            )
          ),
          Comp.renderS(filter, "Filter-switch")(renderFilter),
          Comp.render(complelted, "Clean-complete")(complelted => {
            OptionOps.when(complelted.nonEmpty){
              <.button("clear completed", ^.onClick := fire(ClearComplete))
            }
          })
        )
      )
    }


    Comp.renderIntoDom(
      rootView,
      dom.document.getElementById("app-content"))
  }

}

```
