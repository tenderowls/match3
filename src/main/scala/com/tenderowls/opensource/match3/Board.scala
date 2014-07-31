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
  case class Swap(pos1:Point, pos2:Point) extends BoardOperation
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
  
  case class MatchedCell(pos:Point, value:Cell)

  abstract class Rules {
    val width:Int
    val height:Int
    def randomValue: Cell
    def +(data:BoardData) = {
      Board(this, data)
    }
  }

  type BoardData = Vector[Cell]

  def apply(rules:Rules, data:BoardData) = new Board(rules, data) 
  
  final class Board(val rules:Rules, val data: BoardData) {

    private type Inc = Int => Int

    private def foreach[T](f: (Point, Int) => T, brd:BoardData = data) = {
      val range = 0 until brd.length
      range.view map {
        i => f( Point(i % rules.width, i / rules.width), i)
      }
    }

    @tailrec
    private def genSeq(lst:List[MatchedCell],
                                nx:Inc = x => x, ny:Inc = y => y):List[MatchedCell] = {
      val prev = lst.head
      val x = nx(prev.pos.x)
      val y = ny(prev.pos.y)
      get(x, y) match {
        case Some(next) if prev.value matchWith next =>
          genSeq(MatchedCell(Point(x, y), next) :: lst, nx, ny)
        case _ => lst
      }
    }

    private def getUnsafe(x:Int, y:Int) = data(x + y * rules.width)

    def get(p:Point):Option[Cell] = get(p.x, p.y)

    def get(x: Int, y: Int):Option[Cell] = {
      if (x >= rules.width || y >= rules.height) {
        None
      }
      else {
        Some(getUnsafe(x, y))
      }
    }

    def fillEmptyCells: Board = {
      Board(
        rules = rules,
        data = data map {
          case EmptyCell() => rules.randomValue
          case x => x
        }
      )
    }

    @tailrec
    def stable: Board = {
      matchedSequences().toList match {
        case Nil => this
        case sequences =>
          val replace = sequences.map(sequence => sequence.head).groupBy {
            cell => (cell.pos.x, cell.pos.y)
          }
          val newBoard = foreach {
            case (Point(x, y), i) =>
              replace.get(x, y) match {
                case Some(_) => rules.randomValue
                case None => data(i)
              }
          }
          Board(rules, newBoard.toVector).stable
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
        val mr = List(MatchedCell(point, data(i)))
        genSeq(mr, nx = x => x + 1)
      }
      // Vertical sequences
      val bs = foreach { (point, i) =>
        val mr = List(MatchedCell(point, data(i)))
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
      val cleanBoard = Board(
        rules = rules,
        foreach { (point, i) =>
          val exists = seq.exists {
            case MatchedCell(`point`, _) => true
            case _ => false
          }
          exists match {
            case true => EmptyCell()
            case false => data(i)
          }
        }.toVector
      )
      // Calculate cell transition operations
      val transitionOps = seq filter {
        // First of all, let's keep only those cells which
        // don't have empty neighbour to bottom
        case MatchedCell(Point(x, y), _) =>
          cleanBoard.get(x, y + 1) match {
            case Some(EmptyCell()) => false
            case _ => true
          }
      } map { case MatchedCell(Point(x, y), _) =>
        // Find top boundary for cell from sequence. It can be
        // top boundary of board or BadCell
        val boundary = (-1 to y).reverse find {
          yy => yy == -1 || cleanBoard.getUnsafe(x, yy) == BadCell()
        }
        // Find y coordinates of cells upwards from cell
        val topYs = ((boundary.get + 1) to y).reverse.filter { yy =>
          cleanBoard.getUnsafe(x, yy) match {
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
        matched => Update(matched.pos, EmptyCell())
      }
      updateOps ++ transitionOps.flatten
    }

    def applyOperations(operations:List[BoardOperation]):Board = {
      @tailrec
      def mutateCellAt(point:Point, list:List[BoardOperation]):Cell = list match {
        case x :: xs =>
          x match {
            case Swap(pos1, `point`) => getUnsafe(pos1.x, pos1.y)
            case Swap(`point`, pos2) => getUnsafe(pos2.x, pos2.y)
            case Transition(from, `point`) => getUnsafe(from.x, from.y)
            case Transition(`point`, _) => EmptyCell()
            case Update(`point`, value) => value
            case _ => mutateCellAt(point, xs)
          }
        case _ => getUnsafe(point.x, point.y)
      }
      val reverseOperations = operations.reverse
      val cells = foreach((point, i) => mutateCellAt(point, reverseOperations))
      Board(rules, cells.toVector)
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

  implicit class PointMethods(val point:Point) extends AnyVal {

    def left = Point(point.x - 1, point.y)
    def left(x:Int) = Point(point.x - x, point.y)

    def right = Point(point.x + 1, point.y)
    def right(x:Int) = Point(point.x + x, point.y)

    def top = Point(point.x, point.y - 1)
    def top(x:Int) = Point(point.x, point.y - x)

    def bottom = Point(point.x, point.y + 1)
    def bottom(x:Int) = Point(point.x, point.y + x)

    def direction(to:Point) = {
      if (point.x == to.x)
        Vertical()
      else Horizontal()
    }
  }
}