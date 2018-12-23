package com.tenderowls.match3.server.actors

import akka.actor.typed.{Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors
import com.tenderowls.match3.server.data.Score
import com.tenderowls.match3._

import scala.concurrent.duration._

object GameActor {

  def apply(leftPlayer: Player,
            rightPlayer: Player,
            initialBoard: Board,
            timeout: FiniteDuration,
            match3Rules: Rules,
            maxScore: Int): Behavior[Event] = {

    Behaviors.setup[Event] { ctx =>

      ctx.watch(leftPlayer)
      ctx.watch(rightPlayer)

      leftPlayer ! PlayerActor.Event.GameStarted(
        yourTurn = true,
        board = initialBoard,
        game = ctx.self,//ctx.spawnAdapter((op: BoardOperation.Swap) => Event.MakeMove(leftPlayer, op)),
        opponent = rightPlayer
      )

      rightPlayer ! PlayerActor.Event.GameStarted(
        yourTurn = false,
        board = initialBoard,
        game = ctx.self,//ctx.spawnAdapter((op: BoardOperation.Swap) => Event.MakeMove(rightPlayer, op)),
        opponent = leftPlayer
      )

      val boardActor = {
        val proxy = ctx.messageAdapter { result: BoardActor.Result =>
          Event.MoveResult(result.batch, result.score)
        }
        ctx.spawn(BoardActor(proxy, initialBoard, match3Rules), "board")
      }

      def awaitAnimation(turn: Behavior[Event], counter: Int = 1): Behavior[Event] = {
        Behaviors.receive[Event] {
          case (_, Event.AnimationFinished) if counter == 2 => turn
          case (_, Event.AnimationFinished) => awaitAnimation(turn, counter + 1)
          case _ => Behaviors.same
        }
      } receiveSignal {
        case (_, Terminated(`leftPlayer`)) =>
          rightPlayer ! PlayerActor.Event.YouWin
          Behaviors.stopped
        case (_, Terminated(`rightPlayer`)) =>
          leftPlayer ! PlayerActor.Event.YouWin
          Behaviors.stopped
      }

      def turn(currentPlayer: Player, leftPlayerScore: Score, rightPlayerScore: Score): Behavior[Event] = {
        Behaviors.setup[Event] { ctx =>

          if (leftPlayerScore.exists(_ >= maxScore)) {
            leftPlayer ! PlayerActor.Event.YouWin
            rightPlayer ! PlayerActor.Event.YouLose
            Behaviors.stopped
          }
          else if (rightPlayerScore.exists(_ >= maxScore)) {
            leftPlayer ! PlayerActor.Event.YouLose
            rightPlayer ! PlayerActor.Event.YouWin
            Behaviors.stopped
          }
          else {
            if (currentPlayer == leftPlayer) {
              leftPlayer ! PlayerActor.Event.YourTurn(timeout)
              rightPlayer ! PlayerActor.Event.OpponentTurn(timeout)
            } else {
              rightPlayer ! PlayerActor.Event.YourTurn(timeout)
              leftPlayer ! PlayerActor.Event.OpponentTurn(timeout)
            }

            val timeoutSchedule = ctx.scheduleOnce(timeout, ctx.self, Event.TimeIsOut)

            Behaviors.receive[Event] {
              case (_, Event.TimeIsOut) =>
                leftPlayer  ! PlayerActor.Event.EndOfTurn
                rightPlayer ! PlayerActor.Event.EndOfTurn
                currentPlayer match {
                  case `leftPlayer` => turn(rightPlayer, leftPlayerScore, rightPlayerScore)
                  case `rightPlayer` => turn(leftPlayer, leftPlayerScore, rightPlayerScore)
                }
              case (_, Event.MoveResult(batch, score)) =>
                val event = PlayerActor.Event.MoveResult(batch)
                leftPlayer ! event
                rightPlayer ! event
                // Be ready after animation finished
                leftPlayer ! PlayerActor.Event.EndOfTurn
                rightPlayer ! PlayerActor.Event.EndOfTurn
                currentPlayer match {
                  case `leftPlayer` =>
                    val newLeftPlayerScore = leftPlayerScore + score
                    leftPlayer  ! PlayerActor.Event.CurrentScore(newLeftPlayerScore, rightPlayerScore)
                    rightPlayer ! PlayerActor.Event.CurrentScore(rightPlayerScore, newLeftPlayerScore)
                    awaitAnimation(turn(rightPlayer, newLeftPlayerScore, rightPlayerScore))
                  case `rightPlayer` =>
                    val newRightPlayerScore = rightPlayerScore + score
                    leftPlayer  ! PlayerActor.Event.CurrentScore(leftPlayerScore, newRightPlayerScore)
                    rightPlayer ! PlayerActor.Event.CurrentScore(newRightPlayerScore, leftPlayerScore)
                    awaitAnimation(turn(leftPlayer, leftPlayerScore, newRightPlayerScore))
                }

              case (_, Event.MakeMove(client, op)) =>
                if (client == currentPlayer) {
                  timeoutSchedule.cancel()
                  boardActor ! op
                }
                Behaviors.same[Event]
              case _ =>
                Behaviors.same
            } receiveSignal {
              case (_, Terminated(`leftPlayer`)) =>
                rightPlayer ! PlayerActor.Event.YouWin
                Behaviors.stopped
              case (_, Terminated(`rightPlayer`)) =>
                leftPlayer ! PlayerActor.Event.YouWin
                Behaviors.stopped
            }
          }
        }
      }

      // Give turn to left player
      turn(leftPlayer, Score.empty, Score.empty)
    }
  }

  sealed trait Event

  object Event {

    // From board processor

    /**
      * Board sends move calculation result.
      */
    final case class MoveResult(batch: Batch, score: Score) extends Event

    // From scheduler

    /**
      * Scheduler sends this event when turn
      * should be finished due to timeout.
      */
    case object TimeIsOut extends Event

    // From players

    /**
      * Players send it's decision.
      */
    final case class MakeMove(client: Player, op: BoardOperation.Swap) extends Event

    /**
      * Notify game that animation finished and
      * next turn can be started. Both players should
      * send event to resume game.
      */
    case object AnimationFinished extends Event
  }
}
