package com.tenderowls.match3.server.actors

import akka.typed._
import akka.typed.scaladsl._
import com.tenderowls.match3.server.data.Score
import com.tenderowls.match3.BoardOperation.{Swap, Update}
import com.tenderowls.match3.{Board, BoardAdviser}

import scala.concurrent.duration.FiniteDuration

object PlayerActor {

  def localPlayer[U](name: String)(onEvent: PartialFunction[Event, U]): Behavior[Event] = {
    Actor.immutable[Event] {
      case (_, Event.WhatsYourName(replyTo)) =>
        replyTo ! name
        Actor.same
      case (_, event) =>
        onEvent(event)
        Actor.same
    }
  }

  def bot(botName: String): Behavior[Event] = {

    import BoardAdviser._

    def suggestSwap(board: Board) = {
      def applySwap(swap: Swap) = {
        def aux(acc: Int, board: Board): Int = {
          board.matchedSequence match {
            case Some(matched) =>
              val ops = board.calculateRemoveSequenceOperations(matched)
              val (removeOps, _) = ops.partition(_.isInstanceOf[Update])
              val boardWithEmpties = board.applyOperations(ops)
              aux(acc + removeOps.length, boardWithEmpties)
            case None =>
              acc
          }
        }
        aux(0, board.applyOperations(List(swap))) -> swap
      }
      board.advices
        .toList
        .map(applySwap)
        .sortBy(-_._1)
        .headOption
        .map(_._2)
    }

    Actor.immutable[Event] {
      case (ctx, Event.GameStarted(initialBoard, game, _)) =>
        ctx.watch(game)
        def ready(board: Board): Behavior[Event] = {
          Actor.immutable[Event] { (_, msg) =>
            msg match {
              case Event.EndOfTurn =>
                Actor.same
              case Event.MoveResult(batch) =>
                ready(board.applyOperations(batch.flatten))
              case Event.WhatsYourName(replyTo) =>
                replyTo ! botName
                Actor.same
              case _: Event.YourTurn =>
                suggestSwap(board).foreach { swap =>
                  game ! swap
                }
                Actor.same
              case _: Event.OpponentTurn =>
                Actor.same
              case _: Event.GameStarted =>
                Actor.same
            }
          } onSignal {
            case (_, Terminated(`game`)) =>
              Actor.stopped
          }
        }
        ready(initialBoard)
      case (_, Event.WhatsYourName(replyTo)) =>
        replyTo ! botName
        Actor.same
      case _ =>
        Actor.same
    }
  }

  sealed trait Event

  object Event {
    final case class GameStarted(board: Board, game: ActorRef[Swap], opponent: Player) extends Event
    final case class MoveResult(batch: Batch) extends Event
    final case class YourScore(score: Score) extends Event
    final case class YourTurn(time: FiniteDuration) extends Event
    final case class OpponentTurn(time: FiniteDuration) extends Event
    final case class WhatsYourName(replyTo: ActorRef[String]) extends Event

    case object EndOfTurn extends Event
  }
}
