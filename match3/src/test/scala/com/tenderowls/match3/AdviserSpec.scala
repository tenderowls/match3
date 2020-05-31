package com.tenderowls.match3

import BoardOperation._

import com.tenderowls.match3.BoardGenerator._
import com.tenderowls.match3.BoardAdviser._
import org.specs2._

import scala.util.Random

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
object AdviserSpec extends Specification {

  val rnd = new Random()

  implicit val rndValue = () => IntCell(rnd.nextInt(6))

  def is = s2"""

    Advices

      1) 0 1 2 3 4 5   2) _ _ _ _ _ 2 _ _
         5 3 3 2 1 0      _ 3 _ _ _ 2 _ _
         3 1 2 3 4 5      3 1 _ _ _ 0 2 _
         5 4 3 2 1 0      _ 3 _ _ _ 2 _ _
         0 1 2 3 4 5      _ _ _ _ _ 2 _ _
                          _ _ _ 4 4 4 0 _
                          _ _ _ _ _ _ 4 _
                       
      1) This board has five available moves                                 $testAdvices1
      2) Best advice for this board (with normal heruistic) is 6,2 to 5,2    $testBestAdviceNormal
  """

  def testAdvices1 = {

    val board = board"""
      0 1 2 3 4 5
      5 3 3 2 1 0
      3 1 2 3 4 5
      5 4 3 2 1 0
      0 1 2 3 4 5
    """

    board.advices.size mustEqual 8
  }

  def testBestAdviceNormal = {

    val board = board"""
      _ _ _ _ _ 2 _ _
      _ 3 _ _ _ 2 _ _
      3 1 _ _ _ 0 2 _
      _ 3 _ _ _ 2 _ _
      _ _ _ _ _ 2 _ _
      _ _ _ 4 4 4 0 _
      _ _ _ _ _ _ 4 _
    """

    board.bestAdvice.get mustEqual Swap(Point(6, 2), Point(5, 2))
  }

}
