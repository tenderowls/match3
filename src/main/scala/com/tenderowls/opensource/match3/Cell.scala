package com.tenderowls.opensource.match3

trait Cell {
  def matchWith(x: Cell): Boolean = equals(x)

  override def toString: String = super.toString
}

object Cell {

  case object EmptyCell extends Cell {
    override def matchWith(x: Cell) = false
    override def toString = "*"
  }

  case object BadCell extends Cell {
    override def matchWith(x: Cell) = false
    override def toString = " "
  }

  case class MatchedCell(pos: Point, value: Cell)
}
