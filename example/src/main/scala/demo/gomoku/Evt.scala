package demo.gomoku

import demo.gomoku.Main.Position

trait Evt

case class LuoZi(position: Position) extends Evt

object Rest extends Evt