package demo.gomoku

import demo.gomoku.Main.Position

trait Direction {
  def next(position: Position): Position

  def previous(position: Position): Position
}

object Direction {
  val all: Seq[Direction] = {
    val line = new Direction {
      override def next(position: Position): Position = position.copy(col = position.col + 1)

      override def previous(position: Position): Position = position.copy(col = position.col - 1)
    }

    val col = new Direction {
      override def next(position: Position): Position = position.copy(line = position.line + 1)

      override def previous(position: Position): Position = position.copy(line = position.line - 1)
    }

    val leftInclined = new Direction {
      override def next(position: Position): Position = position.copy(line = position.line + 1, col = position.col + 1)

      override def previous(position: Position): Position = position.copy(line = position.line - 1, col = position.col - 1)
    }

    val rightInclined = new Direction {
      override def next(position: Position): Position = position.copy(line = position.line - 1, col = position.col + 1)

      override def previous(position: Position): Position = position.copy(line = position.line + 1, col = position.col - 1)
    }

    Seq(line, col, leftInclined, rightInclined)
  }
}