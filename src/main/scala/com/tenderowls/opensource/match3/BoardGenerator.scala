package com.tenderowls.opensource.match3

import com.tenderowls.opensource.match3.Board._

import scala.util.Random

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object BoardGenerator {

  case class IntCell(x: Int) extends Cell {
    override def toString = {
      x.toString
    }
  }

  implicit class BoardGeneratorInterpolation(val sc: StringContext) extends AnyVal {

    def board(args: Any*): BoardMethods = {
      val ValuePattern = "(\\d)".r
      val s = sc.parts.mkString
      val rows = s.split("\n")
        .map(row => row.trim)
        .filter(_.length > 0)
      val cells =
        0 until rows.length map { y =>
          val values = rows(y).split(" ")
          0 until values.length filter { x =>
            values(x) match {
              case "" => false
              case _ => true
            }
          } map { x =>
            values(x) match {
              case "_" => BadCell()
              case "*" => EmptyCell()
              case ValuePattern(value) => IntCell(value.toInt)
              case s: String =>
                val code = s.charAt(0).toByte
                throw new Exception(s"Wrong board format. Invalid character `$s` (0x$code)")
            }
          }
        }
      val vec = cells.flatten.toVector
      implicit val rules = new Rules {
        val rnd = new Random()
        override def randomValue: Cell = {
          IntCell(rnd.nextInt(6))
        }
        override val height: Int = rows.length
        override val width: Int = vec.length / height
      }
      new BoardMethods(vec)
    }
  }

}
