package com.tenderowls.opensource.match3

abstract class Rules {

  val width: Int
  val height: Int

  def randomValue: Cell

  def +(data: BoardData): Board = Board(this, data)
}
