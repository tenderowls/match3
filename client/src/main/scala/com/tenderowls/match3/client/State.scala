package com.tenderowls.match3.client

import com.tenderowls.match3.server.data.{PlayerInfo, Score}
import com.tenderowls.match3.client.components.BoardComponent.Params
import korolev._
import korolev.state.javaSerialization._
import korolev.execution._

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

sealed trait State

object State {

  case object Login extends State
  case class LoggedIn(name: String, state: LoggedInState) extends State

  sealed trait LoggedInState
  case object Lobby extends LoggedInState
  case class Game(info: GameInfo, boardParams: Params) extends LoggedInState
  case object YouWin extends LoggedInState
  case object YouLose extends LoggedInState
  case object Draw extends LoggedInState

  case class GameInfo(
    currentPlayer: PlayerInfo,
    you: PlayerInfo,
    opponent: PlayerInfo,
    yourScore: Score,
    opponentScore: Score,
    timeRemaining: Option[FiniteDuration]
  )

  val globalContext = Context[Future, State, ClientEvent]
}
