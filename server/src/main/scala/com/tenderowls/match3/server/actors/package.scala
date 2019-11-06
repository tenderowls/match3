package com.tenderowls.match3.server

import akka.actor.typed.ActorRef
import com.tenderowls.match3.BoardOperation

package object actors {

  type Batch = List[List[BoardOperation]]
  type Player = ActorRef[PlayerActor.Event]
  type Game = ActorRef[GameActor.Event]
  type Lobby = ActorRef[LobbyActor.Event]
}
