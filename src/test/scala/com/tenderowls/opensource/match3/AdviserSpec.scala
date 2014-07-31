package com.tenderowls.opensource.match3

import com.tenderowls.opensource.match3.BoardGenerator._
import com.tenderowls.opensource.match3.BoardAdviser._
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

      0 1 2 3 4 5
      5 3 3 2 1 0
      3 1 2 3 4 5
      5 4 3 2 1 0
      0 1 2 3 4 5

      This board has five available moves $testAdvices1


  """

  def testAdvices1 = {

    val board = board"""
      0 1 2 3 4 5
      5 3 3 2 1 0
      3 1 2 3 4 5
      5 4 3 2 1 0
      0 1 2 3 4 5
    """

    board.advices.foreach( x => println(x))
    board.advices.size mustEqual 5
  }
}
