package com.tenderowls.match3.server.actors

import akka.typed._
import akka.typed.scaladsl._
import com.tenderowls.match3.{BoardGenerator, Rules}

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object LobbyActor {

  def mkId: String = Random.alphanumeric.take(6).mkString

  def apply(timeout: FiniteDuration, animationDuration: FiniteDuration, rules: Rules): Behavior[Event] = {
    def matchMaking(pendingPlayerOpt: Option[Player]): Behavior[Event] = {
      Actor.immutable[Event] {
        case (ctx, Event.Enter(player)) =>
          pendingPlayerOpt match {
            case Some(pendingPlayer) =>
              val board = BoardGenerator.square()(rules)
              val gameBehavior = GameActor(pendingPlayer, player, board, timeout, animationDuration, rules)
              ctx.spawn(gameBehavior, s"game-$mkId")
              matchMaking(None)
            case None =>
              val board = BoardGenerator.square()(rules)
              val bot = ctx.spawn(PlayerActor.bot("bot"), s"bot-$mkId")
              val gameBehavior = GameActor(bot, player, board, timeout, animationDuration, rules)
              ctx.spawn(gameBehavior, s"game-$mkId")
              matchMaking(None)
              //matchMaking(Some(player))
          }
        case (_, Event.Leave(player)) if pendingPlayerOpt.contains(player) =>
          matchMaking(None)
        case _ =>
          Actor.same
      }
    }
    matchMaking(None)
  }

  sealed trait Event

  object Event {
    final case class Enter(player: Player) extends Event
    final case class Leave(player: Player) extends Event
  }
}
