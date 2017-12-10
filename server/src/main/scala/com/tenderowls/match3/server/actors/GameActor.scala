package com.tenderowls.match3.server.actors

import akka.typed._
import akka.typed.scaladsl.Actor
import com.tenderowls.match3.server.data.Score
import com.tenderowls.match3._

import scala.concurrent.duration._

object GameActor {

  def apply(leftPlayer: Player,
            rightPlayer: Player,
            initialBoard: Board,
            timeout: FiniteDuration,
            animationDuration: FiniteDuration,
            match3Rules: Rules): Behavior[Event] = {

    Actor.deferred[Event] { ctx =>

      ctx.watch(leftPlayer)
      ctx.watch(rightPlayer)

      leftPlayer ! PlayerActor.Event.GameStarted(
        board = initialBoard,
        game = ctx.spawnAdapter((op: BoardOperation.Swap) => Event.MakeMove(leftPlayer, op)),
        opponent = rightPlayer
      )

      rightPlayer ! PlayerActor.Event.GameStarted(
        board = initialBoard,
        game = ctx.spawnAdapter((op: BoardOperation.Swap) => Event.MakeMove(rightPlayer, op)),
        opponent = leftPlayer
      )

      val boardActor = {
        val proxy = ctx.spawnAdapter { result: BoardActor.Result =>
          Event.MoveResult(result.batch, result.score)
        }
        ctx.spawn(BoardActor(proxy, initialBoard, match3Rules), "board")
      }

      def awaitAnimation(turn: Behavior[Event]) = {
        println("Became await animation")
        Actor.immutable[Event] {
          case (_, Event.Ready) => turn
          case _                    => Actor.same
        }
      }

      def turn(currentPlayer: Player, leftPlayerScore: Score, rightPlayerScore: Score): Behavior[Event] = {

        Actor.deferred[Event] { ctx =>

          println(s"Turn for $currentPlayer")
          if (currentPlayer == leftPlayer) {
            leftPlayer ! PlayerActor.Event.YourTurn(timeout)
            rightPlayer ! PlayerActor.Event.OpponentTurn(timeout)
          } else {
            rightPlayer ! PlayerActor.Event.YourTurn(timeout)
            leftPlayer ! PlayerActor.Event.OpponentTurn(timeout)
          }
          val timeoutSchedule = ctx.schedule(timeout, ctx.self, Event.TimeIsOut)

          Actor.immutable[Event] {
            case (_, Event.TimeIsOut) =>
              currentPlayer match {
                case `leftPlayer` =>
                  leftPlayer ! PlayerActor.Event.EndOfTurn
                  rightPlayer ! PlayerActor.Event.EndOfTurn
                  turn(rightPlayer, leftPlayerScore, rightPlayerScore)
                case `rightPlayer` =>
                  rightPlayer ! PlayerActor.Event.EndOfTurn
                  leftPlayer ! PlayerActor.Event.EndOfTurn
                  turn(leftPlayer, leftPlayerScore, rightPlayerScore)
              }
            case (_, Event.MoveResult(batch, score)) =>
              val event = PlayerActor.Event.MoveResult(batch)
              leftPlayer ! event
              rightPlayer ! event
              // Be ready after animation finished
              println(s"Schedule next turn on ${animationDuration * batch.length}")
              ctx.schedule(animationDuration * batch.length, ctx.self, Event.Ready)
              val nextTurn = currentPlayer match {
                case `leftPlayer` =>
                  leftPlayer ! PlayerActor.Event.EndOfTurn
                  rightPlayer ! PlayerActor.Event.EndOfTurn
                  turn(rightPlayer, leftPlayerScore, rightPlayerScore + score)
                case `rightPlayer` =>
                  rightPlayer ! PlayerActor.Event.EndOfTurn
                  leftPlayer ! PlayerActor.Event.EndOfTurn
                  turn(leftPlayer, leftPlayerScore + score, rightPlayerScore)
              }
              awaitAnimation(nextTurn)
            case (_, Event.MakeMove(client, op)) =>
              if (client == currentPlayer) {
                timeoutSchedule.cancel()
                boardActor ! op
              }
              Actor.same[Event]
            case _ =>
              Actor.same
          } onSignal {
            case (_, Terminated(`leftPlayer`)) => Actor.stopped
            case (_, Terminated(`rightPlayer`)) => Actor.stopped
          }
        }
      }

      // Give turn to left player
      turn(leftPlayer, Score.empty, Score.empty)
    }
  }

  sealed trait Event

  object Event {
    final case class MoveResult(batch: Batch, score: Score) extends Event
    final case class MakeMove(client: Player, op: BoardOperation.Swap) extends Event
    case object TimeIsOut extends Event
    case object Ready extends Event
  }
}
