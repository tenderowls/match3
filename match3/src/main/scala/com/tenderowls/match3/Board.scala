package com.tenderowls.match3

import scala.annotation.tailrec

final case class Board(rules: Rules, rawData: BoardData) {

  import Cell._
  import BoardOperation._

  private type Inc = Int => Int

  lazy val data: Iterable[(Point, Cell)] = {
    rawData.indices.map { i =>
      Point(i % rules.width, i / rules.width) -> rawData(i)
    }
  }
  
  def mapData[T](f: (Point, Int) => T): Iterable[T] = {
    val range = rawData.indices
    range.view map { i =>
      f(Point(i % rules.width, i / rules.width), i)
    }
  }

  @tailrec
  final private def genSeq(lst: List[MatchedCell],
    nx: Inc = x => x,
    ny: Inc = y => y): List[MatchedCell] = {
    val prev = lst.head
    val x = nx(prev.pos.x)
    val y = ny(prev.pos.y)
    get(x, y) match {
      case Some(next) if prev.value matchWith next =>
        genSeq(MatchedCell(Point(x, y), next) :: lst, nx, ny)
      case _ => lst
    }
  }

  protected def buildWithData(data: BoardData) = Board(rules, data)

  private def getUnsafe(x: Int, y: Int) = rawData(x + y * rules.width)

  def get(p: Point): Option[Cell] = get(p.x, p.y)

  def get(x: Int, y: Int): Option[Cell] = {
    if (x < 0 || y < 0 || x >= rules.width || y >= rules.height) {
      None
    } else {
      Some(getUnsafe(x, y))
    }
  }

  def fillEmptyCells: Board = {
    buildWithData(
      rawData map {
        case EmptyCell => rules.randomValue
        case x         => x
      }
    )
  }

  @tailrec
  final def stable: Board = {
    matchedSequences().toList match {
      case Nil => this
      case sequences =>
        val replace = sequences.map(sequence => sequence.head).groupBy {
          cell =>
            (cell.pos.x, cell.pos.y)
        }
        val newBoard = mapData {
          case (Point(x, y), i) =>
            replace.get(x, y) match {
              case Some(_) => rules.randomValue
              case None    => rawData(i)
            }
        }
        buildWithData(newBoard.toVector).stable
    }
  }

  /**
    * Find homogeneous sequences
    * @param minLength match2, match3
    * @return
    */
  def matchedSequences(minLength: Int = 3): Iterable[List[MatchedCell]] = {
    // Horizontal sequences
    val rs = mapData { (point, i) =>
      val mr = List(MatchedCell(point, rawData(i)))
      genSeq(mr, nx = x => x + 1)
    }
    // Vertical sequences
    val bs = mapData { (point, i) =>
      val mr = List(MatchedCell(point, rawData(i)))
      genSeq(mr, ny = y => y + 1)
    }
    val sequences = rs ++ bs
    sequences
      .toSeq
      .filter(_.size >= minLength)
      .sortBy(-_.size)
  }

  /**
    * First homogeneous sequences of 3 to 5 cells
    * @return
    */
  def matchedSequence: Option[List[MatchedCell]] =
    matchedSequences().headOption

  def calculateRemoveSequenceOperations(
    seq: List[MatchedCell]): List[BoardOperation] = {
    // Create board without cells present in sequence
    val cleanBoard = buildWithData(
      mapData { (point, i) =>
        val exists = seq.exists {
          case MatchedCell(`point`, _) => true
          case _                       => false
        }
        if (exists) EmptyCell
        else rawData(i)
      }.toVector
    )
    // Calculate cell transition operations
    val transitionOps = seq filter {
      // First of all, let's keep only those cells which
      // don't have empty neighbour to bottom
      case MatchedCell(Point(x, y), _) =>
        cleanBoard.get(x, y + 1) match {
          case Some(EmptyCell) => false
          case _               => true
        }
    } map {
                          case MatchedCell(Point(x, y), _) =>
                            // Find top boundary for cell from sequence. It can be
                            // top boundary of board or BadCell
                            val boundary = (-1 to y).reverse find { yy =>
                              yy == -1 || cleanBoard.getUnsafe(x, yy) == BadCell
                            }
                            // Find y coordinates of cells upwards from cell
                            val topYs = ((boundary.get + 1) to y).reverse.filter { yy =>
                              cleanBoard.getUnsafe(x, yy) match {
                                case EmptyCell => false
                                case _         => true
                              }
                            }
                            topYs.indices map { i =>
                              Transition(
                                Point(x, topYs(i)),
                                Point(x, y - i)
                              )
                            }
                        }
    val updateOps = seq map { matched =>
      Update(matched.pos, EmptyCell)
    }
    updateOps ++ transitionOps.flatten
  }

  def applyOperations(operations: List[BoardOperation]): Board = {
    val mutableData = rawData.toArray
    def get(p: Point) = mutableData(p.x + p.y * rules.width)
    def set(p: Point, c: Cell) = mutableData(p.x + p.y * rules.width) = c
    operations foreach {
      case Swap(p1, p2) =>
        val p2value = get(p2)
        set(p2, get(p1))
        set(p1, p2value)
      case Transition(from, to) =>
        set(to, get(from))
        set(from, EmptyCell)
      case Update(p, value) =>
        set(p, value)
    }
    buildWithData(mutableData.toVector)
  }

  def mkString: String = {
    val s = 0 until rules.height map { y =>
      0 until rules.width map { x =>
        getUnsafe(x, y) match {
          case BadCell => "  "
          case cell    => cell.toString + " "
        }
      }
      "\n"
    }
    s.fold("")(_ + _)
  }
}

object Board {
  def apply(rules: Rules, data: BoardData) =
    new Board(rules, data)
}
