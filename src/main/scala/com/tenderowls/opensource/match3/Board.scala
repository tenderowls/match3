package com.tenderowls.opensource.match3

import scala.annotation.tailrec

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object Board {

  sealed trait Direction
  case class Horizontal() extends Direction
  case class Vertical() extends Direction

  trait Cell {
    def matchWith(x: Cell) = {
      equals(x)
    }
    override def toString: String = super.toString
  }

  case class EmptyCell() extends Cell {
    override def matchWith(x: Cell) = false
    override def toString = "* "
  }

  case class BadCell() extends Cell {
    override def matchWith(x: Cell) = false
    override def toString = " "
  }
  
  case class MatchedCell(x:Int, y:Int, value:Cell)

  abstract class Rules {
    def randomValue: Cell
    val width:Int
    val height:Int
  }

  type Board = Vector[Cell]

  def square(stable:Boolean = true)(implicit p: Rules): Board = {
    val raw =
      0 until p.height map { y =>
        0 until p.width map { x =>
          p.randomValue
        }
      }
    val set = raw.flatten.toVector
    stable match {
      case true => set.stable
      case false => set
    }
  }

  implicit class BoardMethods(val board: Board)(implicit rules: Rules) {

    val range = 0 until board.length

    private def calcY(index:Int) = index / rules.width

    private def calcX(index:Int) = index % rules.width

    private def getUnsafe(x:Int, y:Int) = board(x + y * rules.width)

    private type Result = List[MatchedCell]

    private type Inc = Int => Int

    @tailrec private def sequenceInternal(lst:Result, nx:Inc, ny:Inc):Result = {
      val prev = lst.head
      val x = nx(prev.x)
      val y = ny(prev.y)
      apply(x, y) match {
        case Some(next) if prev.value matchWith next =>
          sequenceInternal(MatchedCell(x, y, next) :: lst, nx, ny)
        case _ => lst
      }
    }

    def apply(x: Int, y: Int) = {
      if (x >= rules.width || y >= rules.height) {
        None
      }
      else {
        Some(getUnsafe(x, y))
      }
    }

    def fillEmptyCells: Board = {
      board map {
        case EmptyCell() => rules.randomValue
        case x => x
      }
    }

    @tailrec final def stable: Board = {
      matchedSequences().toList match {
        case Nil => board
        case sequences =>
          val replace = sequences.map(sequence => sequence.head).groupBy( cell => (cell.x, cell.y) )
          val newBoard = range.view map { i =>
            val aX = calcX(i)
            val aY = calcY(i)
            replace.get(aX, aY) match {
              case Some(_) => rules.randomValue
              case None => getUnsafe(aX, aY)
            }
          }
          newBoard.toVector.stable
      }
    }

    /**
     * Find homogeneous sequences
     * @param startLength match2, match3
     * @return
     */
    def matchedSequences(startLength: Int = 3) = {
      val view = range.view
      // Horizontal sequences
      val rs = view.map { i =>
        val mr = List(MatchedCell(calcX(i), calcY(i), board(i)))
        sequenceInternal(mr, x => x + 1, y => y)
      }
      // Vertical sequences
      val bs = view.map { i =>
        val mr = List(MatchedCell(calcX(i), calcY(i), board(i)))
        sequenceInternal(mr, x => x, y => y + 1)
      }
      val sequences = rs ++ bs
      sequences.filter(_.size >= startLength)
    }

    /**
     * First homogeneous sequences of 3 to 5 cells
     * @return
     */
    def matchedSequence = matchedSequences().headOption

    def stringify = {
      val s = new StringBuilder()
      0 until rules.height map { y =>
        0 until rules.width map { x =>
          getUnsafe(x, y) match {
            case BadCell() => s.append("  ")
            case cell => s.append(cell.toString + " ")
          }
        }
        s.append("\n")
      }
      s.toString()
    }
  }
}