package demo.gomoku

import demo.gomoku.Main.Position

object AI {

  case class PiecesPattern(minEmpty: Int, continous: Int, maxEmpty: Int)

  type Pattern = Seq[PiecesPattern] => Boolean

  val willWin = (p: PiecesPattern) => p.continous >= 5
  val threeInline = (p: PiecesPattern) => p.continous == 3 && p.minEmpty > 1
  val fourInLine = (p: PiecesPattern) => p.continous == 4 && p.minEmpty >= 1

  val four = (p: PiecesPattern) => p.continous == 4 && p.maxEmpty >= 1

  val patternScores = Seq[(String, Pattern, (Int, Int))](
    ("Win", pattern => pattern.exists(willWin), 1000 -> 900),
    ("Double 4", p => p.count(four) >= 2, 800 -> 780),
    ("4 3", p => p.exists(four) && p.exists(threeInline), 700 -> 680),
    ("doble 3", p => p.count(threeInline) >= 2, 350 -> 320),
    ("Ai will be 4", pattern => pattern.exists(fourInLine), 300 -> 290),
    ("make ai three", p => p.exists(threeInline), 7 -> 6),
  )

  def bestMove(content: Array[Array[Int]], boardSize: Int, aiValue: Int, playerValue: Int): (Position, Array[Array[Int]]) = {
    //initial score
    val score: Array[Array[Int]] = 0 to boardSize map { _ =>
      0 until boardSize map { _ => 0 } toArray
    } toArray

    def isInside(position: Position) =
      position.line >= 0 && position.line < boardSize && position.col >= 0 && position.col < boardSize

    def vlaueAt(position: Position) = content(position.line)(position.col)

    def getPattern(position: Position, value: Int, direction: Direction): PiecesPattern = {
      val agaistValue = if (value == aiValue) playerValue else aiValue
      var piecesCount = 1
      var leftEmpty = 0
      var rightEmpty = 0

      var break = false
      var p = direction.next(position)

      while (isInside(p) && vlaueAt(p) != agaistValue) {
        if (!break && vlaueAt(p) == value) {
          piecesCount += 1
        } else {
          break = true
        }
        if (break) {
          leftEmpty += 1
        }

        p = direction.next(p)
      }

      break = false
      p = direction.previous(position)
      while (isInside(p) && vlaueAt(p) != agaistValue) {
        if (!break && vlaueAt(p) == value) {
          piecesCount += 1
        } else {
          break = true
        }
        if (break) {
          rightEmpty += 1
        }
        p = direction.previous(p)
      }

      PiecesPattern(Math.min(leftEmpty, rightEmpty), piecesCount, Math.max(leftEmpty, rightEmpty))
    }

    def defaultPatterhScore(patterns: Seq[PiecesPattern]): Int = {
      if (patterns.forall(p => p.continous + p.maxEmpty + p.minEmpty < 5)) 1
      else patterns.map(_.continous).sum
    }

    def scoreAt(line: Int, col: Int, value: Int): Int = {
      val position = Position(line, col)
      val isAi = value == aiValue
      val patterns = Direction.all map { direction =>
        getPattern(position, value, direction)
      }

      patternScores
        .find(_._2(patterns))
        .map(t => if (isAi) t._3._1 else t._3._2)
        .getOrElse(defaultPatterhScore(patterns))
    }

    content.zipWithIndex foreach { t =>
      val (line, lineIndex) = t
      line.zipWithIndex foreach { t =>
        val (value, colIndex) = t
        if (value != aiValue && value != playerValue) {
          score(lineIndex)(colIndex) =
            Math.max(scoreAt(lineIndex, colIndex, aiValue), scoreAt(lineIndex, colIndex, playerValue))
        }
      }
    }

    var maxScore: (Position, Int) = Position(0, 0) -> 0
    score.zipWithIndex foreach { t =>
      val (line, lineIndex) = t
      line.zipWithIndex foreach { t =>
        val (value, colIndex) = t
        if (value > maxScore._2) {
          maxScore = Position(lineIndex, colIndex) -> value
        }
      }
    }
    maxScore._1 -> score
  }

}
