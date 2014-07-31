package com.tenderowls.opensource.match3

import com.tenderowls.opensource.match3.Board._
import com.tenderowls.opensource.match3.BoardGenerator._
import org.specs2._

import scala.util.Random

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object BoardSpec extends Specification {

  val rnd = new Random()

  implicit val rndValue = () => IntCell(rnd.nextInt(6))

  def is = s2"""

    Board matching

      Simple match 3 horizontal in a simple one dimension board           $horizontalMatch
      Match 5 vertical                                                    $verticalMatch
      Match 3 vertical and horizontal                                     $doubleMatch

    When sequence was matched we need to find new state of board.
    There is several reasons (like animation on a client side,
    or client-server messaging) to calculate new board state
    incrementally

      Sequence of four vertical cells           $testSequenceOperationsCalculator1
      Sequence of three horizontal cells        $testSequenceOperationsCalculator2

    More complex cases. Board winth 'bad cells'

      Three vertical cells and 'bad cell' upwards to sequence   $testSequenceOperationsCalculator3
      TODO Physics

    Apply operations tests

      Sequence of four vertical cells           $testApplyOperations1
      Sequence of five horizontal cells         $testApplyOperations2
      Sequence of three vertical and bad cell   $testApplyOperations3
  """

  def horizontalMatch =
    board"0 0 1 1 2 2 2 0".matchedSequence.get.toSet mustEqual Set(
      MatchedCell(Point(6, 0), IntCell(2)),
      MatchedCell(Point(5, 0), IntCell(2)),
      MatchedCell(Point(4, 0), IntCell(2))
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
      MatchedCell(Point(2, 0), IntCell(1)),
      MatchedCell(Point(2, 1), IntCell(1)),
      MatchedCell(Point(2, 2), IntCell(1)),
      MatchedCell(Point(2, 3), IntCell(1)),
      MatchedCell(Point(2, 4), IntCell(1))
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
        MatchedCell(Point(0, 0), IntCell(1)),
        MatchedCell(Point(0, 1), IntCell(1)),
        MatchedCell(Point(0, 2), IntCell(1))
      ),
      Set(
        MatchedCell(Point(0, 0), IntCell(1)),
        MatchedCell(Point(1, 0), IntCell(1)),
        MatchedCell(Point(2, 0), IntCell(1))
      )
    )
  }

  def separateOps(ops:List[BoardOperation]) = {
    val updates = ops filter {
      case x: Update => true
      case _ => false
    }

    val transitions = ops filter {
      case x: Transition => true
      case _ => false
    }
    (updates.toSet, transitions)
  }

  def testSequenceOperationsCalculator1 = {

    val board = board"""
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 3 4 5 6 7 8
      8 7 9 5 4 3 2 1
      1 2 9 4 5 6 7 8
      8 7 9 5 4 3 2 1
      1 2 9 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """

    val seq = board.matchedSequence.get
    val ops = separateOps(board.calculateRemoveSequenceOperations(seq))

    ops._1 mustEqual Set(
      Update(Point(2, 3), EmptyCell()),
      Update(Point(2, 4), EmptyCell()),
      Update(Point(2, 5), EmptyCell()),
      Update(Point(2, 6), EmptyCell())
    )

    ops._2 mustEqual List(
      Transition(Point(2, 2), Point(2, 6)),
      Transition(Point(2, 1), Point(2, 5)),
      Transition(Point(2, 0), Point(2, 4))
    )
  }

  def testSequenceOperationsCalculator2 = {

    val board = board"""
      1 2 3 4 5 6 7 8
      8 7 6 5 4 2 2 1
      1 2 1 3 3 3 7 8
      8 7 6 5 4 2 2 1
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """

    val seq = board.matchedSequence.get
    val ops = separateOps(board.calculateRemoveSequenceOperations(seq))

    ops._1 mustEqual Set(
      Update(Point(3, 2), EmptyCell()),
      Update(Point(4, 2), EmptyCell()),
      Update(Point(5, 2), EmptyCell())
    )

    ops._2 mustEqual List(
      Transition(Point(5, 1), Point(5, 2)),
      Transition(Point(5, 0), Point(5, 1)),
      Transition(Point(4, 1), Point(4, 2)),
      Transition(Point(4, 0), Point(4, 1)),
      Transition(Point(3, 1), Point(3, 2)),
      Transition(Point(3, 0), Point(3, 1))
    )
  }

  def testSequenceOperationsCalculator3 = {

    val board = board"""
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 _ 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 9 4 5 6 7 8
      8 7 9 5 4 3 2 1
      1 2 9 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """

    val seq = board.matchedSequence.get
    val ops = separateOps(board.calculateRemoveSequenceOperations(seq))

    ops._1 mustEqual Set(
      Update(Point(2, 4), EmptyCell()),
      Update(Point(2, 5), EmptyCell()),
      Update(Point(2, 6), EmptyCell())
    )

    ops._2 mustEqual List(
      Transition(Point(2, 3), Point(2, 6))
    )
  }

  def testApplyOperations(board:BoardMethods, expect:BoardMethods) = {
    implicit val rules = board.rules
    val seq = board.matchedSequence.get
    val ops = board.calculateRemoveSequenceOperations(seq)
    val newBoard = board.applyOperations(ops)
    newBoard.stringify mustEqual expect.stringify
  }

  def testApplyOperations1 = testApplyOperations(
    board = board"""
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 3 4 5 6 7 8
      8 7 9 5 4 3 2 1
      1 2 9 4 5 6 7 8
      8 7 9 5 4 3 2 1
      1 2 9 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """,
    expect = board"""
      1 2 * 4 5 6 7 8
      8 7 * 5 4 3 2 1
      1 2 * 4 5 6 7 8
      8 7 * 5 4 3 2 1
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """
  )

  def testApplyOperations2 = testApplyOperations(
    board = board"""
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 3 4 5 6 7 8
      8 7 8 9 9 9 9 9
      1 2 7 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 5 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """,
    expect = board"""
      1 2 3 * * * * *
      8 7 6 4 5 6 7 8
      1 2 3 5 4 3 2 1
      8 7 8 4 5 6 7 8
      1 2 7 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 5 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """
  )

  def testApplyOperations3 = testApplyOperations(
    board = board"""
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 _ 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 9 4 5 6 7 8
      8 7 9 5 4 3 2 1
      1 2 9 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """,
    expect = board"""
      1 2 3 4 5 6 7 8
      8 7 6 5 4 3 2 1
      1 2 _ 4 5 6 7 8
      8 7 * 5 4 3 2 1
      1 2 * 4 5 6 7 8
      8 7 * 5 4 3 2 1
      1 2 6 4 5 6 7 8
      8 7 6 5 4 3 2 1
    """
  )
}
