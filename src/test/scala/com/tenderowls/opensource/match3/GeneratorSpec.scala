package com.tenderowls.opensource.match3

import com.tenderowls.opensource.match3.Board._
import com.tenderowls.opensource.match3.BoardGenerator._
import org.specs2._

import scala.util.Random

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object GeneratorSpec extends Specification {

  val rnd = new Random()

  implicit val rndValue = () => IntCell(rnd.nextInt(6))

  def is = s2"""

    Check board generator

      Simple 1D board         $simpleBoard
      Simple 2D board         $board2d

    However there is no more important cases
  """

  def simpleBoard = board"0 0 1".board mustEqual Vector(
    IntCell(0),
    IntCell(0),
    IntCell(1)
  )

  def board2d = {
    val board = board"""
      0 0 1
      * * 0
      0 0 _
      """
    board.board mustEqual Vector(
      IntCell(0), IntCell(0), IntCell(1),
      EmptyCell(), EmptyCell(), IntCell(0),
      IntCell(0), IntCell(0), BadCell()
    )
  }

}
