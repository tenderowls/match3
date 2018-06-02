package com.tenderowls.match3.client

import com.tenderowls.match3.BoardOperation

sealed trait ClientEvent

object ClientEvent {
  case class EnterLobby(name: String) extends ClientEvent
  case object LeaveLobby extends ClientEvent
  case class MakeMove(swap: BoardOperation.Swap) extends ClientEvent
  case object PlayWithBot extends ClientEvent
}
