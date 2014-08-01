package com.tenderowls.opensource.match3

import com.tenderowls.opensource.match3.Board._

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object BoardAdviser {

  def normalHeuristic(brd:Board, swp:Swap):Int = {
    brd.applyOperations(List(swp)).matchedSequences().flatten.size
  }

  implicit class BoardAdviserMethods(val board:Board) {

    private def f(matched:MatchedCell)(lookup: Point => Point) = {
      board.get(lookup(matched.pos)) match {
        case Some(cell) if cell matchWith matched.value => true
        case _ => false
      }
    }

    def adviceSpecificSequences:Iterable[List[MatchedCell]] = {
      val raw = board.foreach { (firstPoint:Point, i) =>
        board.get(firstPoint) match {
          case Some(firstCell) =>
            def genSeq(secondPoint:Point):List[MatchedCell] = {
              board.get(secondPoint) match {
                case Some(secondCell) =>
                  firstCell matchWith secondCell match {
                    case true =>
                      List(
                        MatchedCell(secondPoint, secondCell),
                        MatchedCell(firstPoint, firstCell)
                      )
                    case false => List()
                  }
                case None => List()
              }
            }
            List(genSeq(firstPoint right 2), genSeq(firstPoint bottom 2))
          case None => List()
        }
      }
      raw.flatten.filter(_.size == 2)
    }

    def advices:Iterable[Swap] = {
      advices(board.matchedSequences(2) ++ adviceSpecificSequences)
    }

    def advices(sequences:Iterable[List[MatchedCell]]):Iterable[Swap] = {
      val ret = sequences map { seq =>
        val reversedSeq = seq.reverse
        val fst = reversedSeq(0)
        val snd = reversedSeq(1)
        fst.pos direction snd.pos match {
          case Horizontal() =>
            if (snd.pos.x - fst.pos.x > 1) {
              lookupTornHorizontal.find(f(fst)(_)) match {
                case Some(lookup) => List(Swap(lookup(fst.pos), fst.pos.right))
                case None => List[Swap]()
              }
            }
            else {
              val left = lookupLeft.find(f(fst)(_)) match {
                case Some(lookup) => List(Swap(lookup(fst.pos), fst.pos.left))
                case None => List[Swap]()
              }
              val right = lookupRight.find(f(snd)(_)) match {
                case Some(lookup) => List(Swap(lookup(snd.pos), snd.pos.right))
                case None => List[Swap]()
              }
              left ++ right
            }
          case Vertical() =>
            if (snd.pos.y - fst.pos.y > 1) {
              lookupTornVertical.find(f(fst)(_)) match {
                case Some(lookup) => List(Swap(lookup(fst.pos), fst.pos.bottom))
                case None => List[Swap]()
              }
            }
            else {
              val top = lookupTop.find(f(fst)(_)) match {
                case Some(lookup) => List(Swap(lookup(fst.pos), fst.pos.top))
                case None => List[Swap]()
              }
              val bottom = lookupBottom.find(f(snd)(_)) match {
                case Some(lookup) => List(Swap(lookup(snd.pos), snd.pos.bottom))
                case None => List[Swap]()
              }
              top ++ bottom
            }
        }
      }
      ret.flatten
    }

    def bestAdvice(depth:Int,
                   advices:Iterable[Swap],
                   heuristic: (Board, Swap) => Int):Option[Swap] = {

      val sliced = advices.grouped(depth)
      if (sliced.hasNext) {
        val withWeights = sliced.next map {
          (swap) => (heuristic(board, swap), swap)
        }
        withWeights
          .toList
          .sortBy(_._1)
          .reverse
          .map(_._2)
          .headOption
      }
      else None
    }

    def bestAdvice:Option[Swap] = bestAdvice(default_depth, advices, normalHeuristic)

    def bestAdvice(depth:Int):Option[Swap] = bestAdvice(depth, this.advices, normalHeuristic)

    def bestAdvice(depth:Int, advices:Iterable[Swap]):Option[Swap] = {
      bestAdvice(depth, advices, normalHeuristic)
    }
  }

  private val default_depth = 9

  private val lookupLeft = List(
    (p:Point) => p.left(2),
    (p:Point) => p.left.top,
    (p:Point) => p.left.bottom
  )

  private val lookupRight = List(
    (p:Point) => p.right(2),
    (p:Point) => p.right.top,
    (p:Point) => p.right.bottom
  )

  private val lookupTop = List(
    (p:Point) => p.top(2),
    (p:Point) => p.top.left,
    (p:Point) => p.top.right
  )

  private val lookupBottom = List(
    (p:Point) => p.bottom(2),
    (p:Point) => p.bottom.left,
    (p:Point) => p.bottom.right
  )

  private val lookupTornHorizontal = List(
    (p:Point) => p.right.top,
    (p:Point) => p.right.bottom
  )

  private val lookupTornVertical = List(
    (p:Point) => p.bottom.left,
    (p:Point) => p.bottom.right
  )
}
