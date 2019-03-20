package demo.gomoku

import csw.sjreact.dsl.EventAction.fire
import csw.sjreact.dsl.{<, VDom, ^}
import csw.sjreact.react.Comp
import csw.sjreact.util.OptionOps
import csw.sjreact.{EventChannel, State, state}
import org.scalajs.dom

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Main {
  val EMPTY = 0
  val WHITE = 1
  val BLACK = 2


  case class Gezi(position: Position, ct: Int, win: Boolean = false)

  case class Position(line: Int, col: Int)


  class BoardBackend(val siz: Int = 10) {
    def newLine(line: Int) = 0.until(siz).map { i => Gezi(Position(line, i), EMPTY) } toVector

    val lines: IndexedSeq[State[Vector[Gezi]]] = 0.until(siz).map(i => state(newLine(i)))

    def reset(): Unit = {
      lines.zipWithIndex.foreach { t =>
        val (lineSt, index) = t
        lineSt.set(newLine(index))
      }
    }

    def isInside(position: Position): Boolean = {
      position.line >= 0 && position.line < siz && position.col >= 0 && position.col < siz
    }

    def valueAt(position: Position): Int = {
      lines(position.line).value(position.col).ct
    }

    def udpateValueAt(position: Position, value: Int): Unit = {
      val gezi = lines(position.line).value(position.col)
      line(position.line).update(_.updated(position.col, gezi.copy(ct = value)))
    }

    def line(lineIndex: Int): State[Vector[Gezi]] = lines(lineIndex)
  }

  def run(): Unit = {
    val aiTurn = BLACK

    val currentTurn = state(WHITE)
    val backend = new BoardBackend(15)
    val winner: State[Option[String]] = state(None)
    val aiMoving = state(false)
    val end = state(false)
    val aiScore: State[Option[Array[Array[Int]]]] = state(None)

    def winnersAtDirection(position: Position, direction: Direction): Seq[Position] = {
      val target = backend.valueAt(position)

      val buffer: ArrayBuffer[Position] = ArrayBuffer.empty[Position]
      buffer.append(position)
      var p = direction.next(position)
      while (backend.isInside(p) && backend.valueAt(p) == target) {
        buffer.append(p)
        p = direction.next(p)
      }
      p = direction.previous(position)
      while (backend.isInside(p) && backend.valueAt(p) == target) {
        buffer.append(p)
        p = direction.previous(p)
      }

      if (buffer.size >= 5) buffer.toList
      else Seq.empty
    }

    //player is white
    def ai: Position = {
      val content: Array[Array[Int]] = backend.lines.map(_.value.map(_.ct).toArray).toArray

      val ai = AI.bestMove(content, backend.siz, BLACK, WHITE)
      aiScore.set(Some(ai._2))
      ai._1
    }

    def getWinners(position: Position) = Direction.all.flatMap(d => winnersAtDirection(position, d))


    def win(positions: Seq[Position]) = {
      backend.lines.foreach { line =>
        line.update(gezis => gezis.map { gezi =>
          if (positions.contains(gezi.position)) gezi.copy(win = true)
          else gezi
        })
      }
      winner := Some(if (backend.valueAt(positions.head) == 1) "white" else "black")
      end := true
    }

    def changeTurn(channel: EventChannel[Evt]): Unit = {
      currentTurn.update(v => if (v == WHITE) BLACK else WHITE)

      if (currentTurn.value == aiTurn) {
        aiMoving := true
        Future {
          val aiPosition = ai
          channel.fireEvent(LuoZi(aiPosition))
        }
      } else {
        aiMoving := false
      }
    }

    implicit val channel: EventChannel[Evt] = new EventChannel[Evt] {
      override def processor: PartialFunction[Evt, Any] = {
        case LuoZi(position) =>
          if (backend.valueAt(position) == EMPTY) {
            backend.udpateValueAt(position, currentTurn.value)

            val positions = getWinners(position)
            if (positions.nonEmpty) {
              win(positions)
            } else {
              changeTurn(this)
            }
          }
        case Rest =>
          backend.reset()
          winner.set(None)
          currentTurn := WHITE
          aiMoving := false
          end := false
      }
    }


    def getClz(who: Int) = if (who == WHITE) "white" else "black"

    def renderGezi(gezi: Gezi, score: Int): VDom.Element =
      <.div(
        ^.`class` := "gezi",
        score.toString,
        <.div(^.`class` := "qc",
          ^.onClick :=? OptionOps.when(gezi.ct == EMPTY)(fire(LuoZi(gezi.position))),
          ^.`class` := (gezi.ct match {
            case WHITE => "qizi white"
            case BLACK => "qizi black"
            case _ => "empty"
          }),
          ^.`class` :=? OptionOps.when(gezi.win)("win"))
      )

    def renderQipan =
      Comp.render(aiScore) { ai =>
        <.div(^.`class` := "qipan")(
          backend.lines.map { line =>
            Comp.render(line) { gezis =>
              <.div(^.`class` := "line", <.div(^.`class` := "gezi"))(
                gezis map { ge => renderGezi(ge, ai.map(_ (ge.position.line)(ge.position.col)).getOrElse(0)) }: _*
              )
            }: VDom.Node
          }: _*
        )(
          <.div(^.`class` := "line") ( 0 to 15 map { _ => <.div(^.`class` := "gezi") }: _*))
      }


    def renderHeader =
      Comp.render(currentTurn ~ winner ~ aiMoving ~ end) { t4 =>
        val (who, winner, aiMoving, isEnd) = t4
        val disable = winner.isDefined || aiMoving || isEnd
        <.div(
          ^.`class` := "ct",
          ^.`class` := getClz(who),
          ^.`class` :=? OptionOps.when(disable)("disable"),
          <.div(^.`class` := "indicator",
            <.div(^.`class` := "qc white"),
            <.div(^.`class` := "qc black")
          )
        )
      }


    Comp.renderIntoDom(
      Comp.render(winner) { winner =>
        <.div(^.`class` := "container",
          renderHeader,
          renderQipan,
          winner.map(_ => <.div(<.button("reset", ^.onClick := fire(Rest))))
        )
      },
      dom.document.getElementById("app-content")
    )
  }

}
