package com.tenderowls.match3

sealed trait BoardOperation

object BoardOperation {

  case class Transition(from: Point, to: Point) extends BoardOperation

  case class Swap(pos1: Point, pos2: Point) extends BoardOperation

  case class Update(position: Point, value: Cell) extends BoardOperation

}
