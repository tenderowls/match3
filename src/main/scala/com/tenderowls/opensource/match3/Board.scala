package com.tenderowls.opensource.match3

import scala.annotation.tailrec

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object Board {

  case class Point(x:Int, y:Int)

  sealed trait Direction
  case class Horizontal() extends Direction
  case class Vertical() extends Direction

  sealed trait BoardOperation
  case class Transition(from:Point, to:Point) extends BoardOperation
  case class Update(position:Point, value:Cell) extends BoardOperation

  trait Cell {
    def matchWith(x: Cell) = {
      equals(x)
    }
    override def toString: String = super.toString
  }

  case class EmptyCell() extends Cell {
    override def matchWith(x: Cell) = false
    override def toString = "*"
  }

  case class BadCell() extends Cell {
    override def matchWith(x: Cell) = false
    override def toString = " "
  }
  
  case class MatchedCell(position:Point, value:Cell)

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

  implicit class BoardMethods(val board: Board)(implicit val rules: Rules) {

    private type Inc = Int => Int

    private def foreach[T](f: (Point, Int) => T, brd:Board = board) = {
      val range = 0 until brd.length
      range.view map {
        i => f( Point(i % rules.width, i / rules.width), i)
      }
    }

    @tailrec
    private def genSeq(lst:List[MatchedCell],
                                nx:Inc = x => x, ny:Inc = y => y):List[MatchedCell] = {
      val prev = lst.head
      val x = nx(prev.position.x)
      val y = ny(prev.position.y)
      apply(x, y) match {
        case Some(next) if prev.value matchWith next =>
          genSeq(MatchedCell(Point(x, y), next) :: lst, nx, ny)
        case _ => lst
      }
    }

    private def getUnsafe(x:Int, y:Int, brd:Board = board) = brd(x + y * rules.width)

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
          val replace = sequences.map(sequence => sequence.head).groupBy {
            cell => (cell.position.x, cell.position.y)
          }
          val newBoard = foreach {
            case (Point(x, y), i) =>
              replace.get(x, y) match {
                case Some(_) => rules.randomValue
                case None => board(i)
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
      // Horizontal sequences
      val rs = foreach { (point, i) =>
        val mr = List(MatchedCell(point, board(i)))
        genSeq(mr, nx = x => x + 1)
      }
      // Vertical sequences
      val bs = foreach { (point, i) =>
        val mr = List(MatchedCell(point, board(i)))
        genSeq(mr, ny = y => y + 1)
      }
      val sequences = rs ++ bs
      sequences.filter(_.size >= startLength)
    }

    /**
     * First homogeneous sequences of 3 to 5 cells
     * @return
     */
    def matchedSequence = matchedSequences().headOption

    def calculateRemoveSequenceOperations(seq:List[MatchedCell]):List[BoardOperation] = {
      // Create board without cells present in sequence
      val cleanBoard = foreach { (point, i) =>
        val exists = seq.exists {
          case MatchedCell(`point`, _) => true
          case _ => false
        }
        exists match {
          case true => EmptyCell()
          case false => board(i)
        }
      }.toVector
      // Calculate cell transition operations
      val transitionOps = seq filter {
        // First of all, let's keep only those cells which
        // don't have empty neighbour to bottom
        case MatchedCell(Point(x, y), _) =>
          cleanBoard.apply(x, y + 1) match {
            case Some(EmptyCell()) => false
            case _ => true
          }
      } map { case MatchedCell(Point(x, y), _) =>
        // Find top boundary for cell from sequence. It can be
        // top boundary of board or BadCell
        val boundary = (-1 to y).reverse find {
          yy => yy == -1 || getUnsafe(x, yy, cleanBoard) == BadCell()
        }
        // Find y coordinates of cells upwards from cell
        val topYs = ((boundary.get + 1) to y).reverse.filter { yy =>
          getUnsafe(x, yy, cleanBoard) match {
            case EmptyCell() => false
            case _ => true
          }
        }
        0 until topYs.length map {
          i => Transition(
            Point(x, topYs(i)),
            Point(x, y - i)
          )
        }
      }
      val updateOps = seq map {
        matched => Update(matched.position, EmptyCell())
      }
      updateOps ++ transitionOps.flatten
    }

    def applyOperations(operations:List[BoardOperation]) = {
      @tailrec
      def mutateCellAt(point:Point, list:List[BoardOperation]):Cell = list match {
        case x :: xs =>
          x match {
            case Transition(from, `point`) => getUnsafe(from.x, from.y)
            case Transition(`point`, _) => EmptyCell()
            case Update(`point`, value) => value
            case _ => mutateCellAt(point, xs)
          }
        case _ => getUnsafe(point.x, point.y)
      }
      val reverseOperations = operations.reverse
      val cells = foreach((point, i) => mutateCellAt(point, reverseOperations))
      cells.toVector
    }

    def stringify = {
      val s = 0 until rules.height map { y =>
        0 until rules.width map { x =>
          getUnsafe(x, y) match {
            case BadCell() => "  "
            case cell => cell.toString + " "
          }
        }
        "\n"
      }
      s.fold("")(_+_)
    }
  }
}