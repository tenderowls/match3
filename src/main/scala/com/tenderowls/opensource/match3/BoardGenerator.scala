package com.tenderowls.opensource.match3

import com.tenderowls.opensource.match3.Board._

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object BoardGenerator {

  case class IntCell(x: Int) extends Cell {
    override def toString = {
      x.toString
    }
  }

  val ValuePattern = "(\\d)".r

  implicit class BoardGeneratorInterpolation(val sc: StringContext) extends AnyVal {

    def board(args: Any*)(implicit rndVal: () => Cell): Board = {
      val s = sc.parts.mkString
      val rows = s.split("\n")
        .map(row => row.trim)
        .filter(_.length > 0)
      val cells =
        rows.indices map { y =>
          val values = rows(y).split(" ")
          0 until values.length filter { x =>
            values(x) match {
              case "" => false
              case _ => true
            }
          } map { x =>
            values(x) match {
              case "_" => BadCell
              case "*" => EmptyCell
              case "?" => rndVal()
              case ValuePattern(value) => IntCell(value.toInt)
              case s: String =>
                val code = s.charAt(0).toByte
                throw new Exception(s"Wrong board format. Invalid character `$s` (0x$code)")
            }
          }
        }
      val data = cells.flatten.toVector
      val rules = new Rules {
        override def randomValue: Cell = rndVal()
        override val height: Int = rows.length
        override val width: Int = data.length / height
      }
      new Board(rules, data)
    }
  }

  def square(makeStable:Boolean = true)(implicit rules: Rules): Board = {
    val raw =
      0 until rules.height map { y =>
        0 until rules.width map { x =>
          rules.randomValue
        }
      }
    val data = raw.flatten.toVector
    makeStable match {
      case true => Board(rules, data).stable
      case false => Board(rules, data)
    }
  }
}
