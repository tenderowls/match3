package com.tenderowls.opensource.match3

import com.tenderowls.opensource.match3.Board._
import com.tenderowls.opensource.match3.BoardGenerator._

import org.specs2._

import scala.util.Random

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object BoardSpec extends Specification {

  implicit object rules extends Rules {
    val rnd = new Random()
    override val width: Int = 8
    override val height: Int = 8
    override def randomValue: Cell = {
      IntCell(rnd.nextInt(6))
    }
  }

  def is = s2"""

    Check board generator

      simple board            $simpleBoard
      board 2d                $board2d

    Board matching

      simple match 3 horizontal in a simple one dimension board           $horizontalMatch
      match 5 vertical                                                    $verticalMatch
      match 3 vertical and horizontal                                     $doubleMatch
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

  def horizontalMatch =
    board"0 0 1 1 2 2 2 0".matchedSequence.get.toSet mustEqual Set(
      MatchedCell(6, 0, IntCell(2)),
      MatchedCell(5, 0, IntCell(2)),
      MatchedCell(4, 0, IntCell(2))
    )

  def verticalMatch = {

    val board = board"""
      * * 1 *
      * * 1 *
      * * 1 *
      * * 1 *
      * * 1 *
    """

    board.matchedSequence.get.toSet mustEqual Set(
      MatchedCell(2, 0, IntCell(1)),
      MatchedCell(2, 1, IntCell(1)),
      MatchedCell(2, 2, IntCell(1)),
      MatchedCell(2, 3, IntCell(1)),
      MatchedCell(2, 4, IntCell(1))
    )
  }

  def doubleMatch = {

    val board = board"""
      1 1 1
      1 0 0
      1 0 0
    """

    val sequences = board.matchedSequences().toSet
    sequences.map(_.toSet) mustEqual Set(
      Set(
        MatchedCell(0, 0, IntCell(1)),
        MatchedCell(0, 1, IntCell(1)),
        MatchedCell(0, 2, IntCell(1))
      ),
      Set(
        MatchedCell(0, 0, IntCell(1)),
        MatchedCell(1, 0, IntCell(1)),
        MatchedCell(2, 0, IntCell(1))
      )
    )
  }


}
