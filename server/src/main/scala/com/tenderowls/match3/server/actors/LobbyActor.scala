package com.tenderowls.match3.server.actors

import akka.actor.typed.{ Behavior, Terminated }
import akka.actor.typed.scaladsl.Behaviors
import com.tenderowls.match3.{ BoardGenerator, Rules }

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object LobbyActor {

  def generateId: String = Random.alphanumeric.take(6).mkString

  def apply(timeout: FiniteDuration, rules: Rules, maxScore: Int): Behavior[Event] = {
    def matchMaking(pendingPlayers: List[Player]): Behavior[Event] = {
      Behaviors.receive[Event] {
        case (ctx, Event.Enter(player)) =>
          ctx.watch(player)
          pendingPlayers match {
            case Nil =>
              matchMaking(List(player))
            case pendingPlayer :: restPendingPlayers =>
              val board = BoardGenerator.square()(rules)
              val gameBehavior = GameActor(pendingPlayer, player, board, timeout, rules, maxScore)
              ctx.spawn(gameBehavior, s"game-$generateId")
              matchMaking(restPendingPlayers)
          }
        case (_, Event.Leave(player)) =>
          matchMaking(pendingPlayers.filter(_ != player))
        case _ =>
          Behaviors.same
      } receiveSignal {
        case (_, Terminated(player)) =>
          matchMaking(pendingPlayers.filter(_ != player))
      }
    }
    matchMaking(Nil)
  }

  sealed trait Event

  object Event {
    final case class Enter(player: Player) extends Event
    final case class Leave(player: Player) extends Event
  }
}
