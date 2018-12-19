package com.tenderowls.match3.server.actors

import akka.actor.typed._
import akka.actor.typed.scaladsl.Behaviors
import com.tenderowls.match3.server.data.{ColorCell, Score}
import com.tenderowls.match3.BoardOperation.{Swap, Update}
import com.tenderowls.match3._

object BoardActor {

  def apply(client: ActorRef[Result], board: Board, rules: Rules): Behavior[BoardOperation.Swap] = {

    def processSwap(board: Board, swap: Swap): Result = {

      val swapOperation = List(swap)

      val (resultOperations, score, resultBoard) = {
        def aux(acc: Batch, score: Score, board: Board): (Batch, Score, Board) = {
          board.matchedSequence match {
            case None => (acc, score, board)
            case Some(matched) =>
              val ops = board.calculateRemoveSequenceOperations(matched)
              val (removeOps, transitOps) = ops.partition(_.isInstanceOf[Update])
              aux(
                acc = acc :+ removeOps :+ transitOps,
                board = board.applyOperations(ops),
                score = score + removeOps.foldLeft(Score.empty) {
                  case (total, BoardOperation.Update(point, Cell.EmptyCell)) =>
                    board.get(point).fold(total) {
                      case cell: ColorCell => total.inc(cell)
                      case _ => total
                    }
                  case (total, _) => total
                }
              )
          }
        }
        aux(Nil, Score.empty, board.applyOperations(swapOperation))
      }

      val fillOperations = {
        def aux(): List[BoardOperation] = {
          val ops = resultBoard
            .mapData {
              case (p, i) if resultBoard.rawData(i) == Cell.EmptyCell => Some(p)
              case _ => None
            }
            .toList
            .flatten
            .map { p => Update(p, rules.randomValue) }

          // Test board is stable (no matched sequences)
          resultBoard.applyOperations(ops).matchedSequence match {
            case None => ops
            case _ => aux()
          }
        }
        aux()
      }

      Result(
        swapOperation +: resultOperations :+ fillOperations,
        score
      )
    }

    Behaviors.receive {
      case (_, swap) ⇒
        val result = processSwap(board, swap)
        val updatedBoard = board.applyOperations(result.batch.flatten)
        client ! result
        BoardActor(client, updatedBoard, rules)
    }
  }

  case class Result(batch: Batch, score: Score)
}
