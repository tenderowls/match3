package com.tenderowls.match3.client

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.actor.{ActorSystem}
import akka.actor.typed.Scheduler
import akka.util.Timeout
import com.tenderowls.match3.client.State.GameInfo
import com.tenderowls.match3.client.components.BoardComponent
import com.tenderowls.match3.server.actors._
import com.tenderowls.match3.server.data.{PlayerInfo, Score}
import com.tenderowls.match3.{BoardGenerator, BoardOperation, Rules}
import korolev.{Context, Extension}
import akka.actor.typed.scaladsl.adapter._

import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

final class PlayerProxyExtension(lobby: Lobby,
                                 rules: Rules,
                                 gameTimeout: FiniteDuration,
                                 maxScore: Int)
                                (implicit actorSystem: ActorSystem,
                                 scheduler: Scheduler) extends Extension[Future, State, ClientEvent] {

  type Access = Context.BaseAccess[Future, State, ClientEvent]

  class Actors(playerId: String, access: Access)(implicit scheduler: Scheduler) {

    type Proxy = ActorRef[ClientEvent]

    object Proxy {

      // We can't create a PlayerActor because we don't know
      // player name. So we awaiting for EnterLobby(playerName)
      // to spawn PlayerActor
      def apply(access: Access): Behavior[ClientEvent] =
        Behaviors.receive {
          case (_, ClientEvent.EnterLobby(playerName)) =>
            Proxy(playerName)
          case (ctx, event) =>
            ctx.log.error(s"Unexpected client event $event in default state")
            Behaviors.stopped
        }

      // Now when we know player name,
      // we can create player actor
      def apply(playerName: String): Behavior[ClientEvent] =
        Behaviors.setup { ctx =>
          val mediator = ctx.spawnAnonymous(Behaviors.receiveMessagePartial(PlayerMediator.default))
          val player = ctx.spawn(Player(playerName, access, mediator), playerId)
          lobby ! LobbyActor.Event.Enter(player)
          ctx.watch(player) // Looking if player actor is stopped
          ctx.watch(mediator) // Looking if player actor is stopped
          Behaviors.receiveMessage[ClientEvent] {
            case ClientEvent.PlayWithBot =>
              // User want to play with bot.
              // Lets spawna game and exit lobby.
              val board = BoardGenerator.square()(rules)
              val bot = ctx.spawn(PlayerActor.bot("bot"), s"bot-${LobbyActor.generateId}")
              val gameBehavior = GameActor(player, bot, board, gameTimeout, rules, maxScore)
              lobby ! LobbyActor.Event.Leave(player)
              ctx.spawn(gameBehavior, s"game-${LobbyActor.generateId}")
              Behaviors.same
            case ClientEvent.SyncAnimation =>
              mediator ! PlayerMediator.Event.SyncAnimation
              Behaviors.same
            case ClientEvent.EnterLobby(_) =>
              lobby ! LobbyActor.Event.Enter(player)
              Behaviors.same
            case ClientEvent.MakeMove(swap) =>
              mediator ! PlayerMediator.Event.MakeMove(swap)
              Behaviors.same
          } receiveSignal {
            case (_, Terminated(`player`)) =>
              ctx.log.error(s"$player has been stopped accidentally")
              access.transition(_ => State.Login)
              Behaviors.stopped
            case (_, Terminated(`mediator`)) =>
              ctx.log.error(s"$mediator has been stopped accidentally")
              access.transition(_ => State.Login)
              Behaviors.stopped
          }
        }
    }

    type PlayerMediator = ActorRef[PlayerMediator.Event]

    object PlayerMediator {

      val default: PartialFunction[Event, Behavior[Event]] = {
          case Event.InGame(player, game) =>
            apply(player, game)
        }

      def apply(player: Player, game: Game): Behavior[Event] = Behaviors.setup { ctx =>
        def aux(animated: Boolean, queue: Queue[Batch]): Behavior[Event] =
          Behaviors.receiveMessage {
            default orElse {
              case Event.MakeMove(swap) =>
                game ! GameActor.Event.MakeMove(player, swap)
                Behaviors.same
              case Event.MoveResult(batch) if animated =>
                aux(animated = true, queue.enqueue(batch))
              case Event.MoveResult(batch) if !animated =>
                applyBatch(batch)
                aux(animated = true, Queue.empty)
              case Event.SyncAnimation if queue.nonEmpty =>
                val (batch, tl) = queue.dequeue
                applyBatch(batch)
                game ! GameActor.Event.AnimationFinished
                aux(animated, tl)
              case Event.SyncAnimation =>
                game ! GameActor.Event.AnimationFinished
                aux(animated = false, Queue.empty)
              case event =>
                ctx.log.error(s"Unexpected $event. Already initialized.")
                Behaviors.stopped
            }
          }
        aux(animated = false, Queue.empty)
      }

      sealed trait Event

      object Event {
        final case class MoveResult(batch: Batch) extends Event
        final case class MakeMove(swap: BoardOperation.Swap) extends Event
        final case class InGame(player: Player, game: Game) extends Event
        case object SyncAnimation extends Event
      }
    }

    /**
     * Actor transforms application state according to player events.
     *
     * Lifecycle:
     *   Start -> WhatsYourName, GameStarted)
     *         -> SyncAnimation, MoveResult, CurrentScore,
     *            YourTurn, OpponentTurn, WhatsYourName,
     *    Stop <- YouWin, YouLose, Draw, EndOfTurn
     */
    def Player(playerName: String,
               access: Access,
               mediator: PlayerMediator)
              (implicit scheduler: Scheduler): Behavior[PlayerActor.Event] = Behaviors.setup { ctx =>

      val playerRespondNameBehavior: PartialFunction[PlayerActor.Event, Behavior[PlayerActor.Event]] = {
          case PlayerActor.Event.WhatsYourName(replyTo) =>
            replyTo ! playerName
            Behaviors.same
          case _: PlayerActor.Event.CurrentScore =>
            // Ignore score event.
            // For the view score accruing synchronized with animation.
            Behaviors.same
        }

      lazy val playerAwaitGameBehavior: Behavior[PlayerActor.Event] = Behaviors.setup { ctx =>
        Behaviors.receiveMessagePartial {
          playerRespondNameBehavior orElse {
            case PlayerActor.Event.GameStarted(yourTurn, board, gameRef, opponent) =>
              implicit val timeout: Timeout = Timeout(5.seconds)
              opponent
                .ask(PlayerActor.Event.WhatsYourName)
                .map { opponentName: String =>
                  transformState { _ =>
                    val you = PlayerInfo(playerName)
                    val enemy = PlayerInfo(opponentName)
                    val currentPlayer = if (yourTurn) you else enemy
                    val info = GameInfo(currentPlayer, you, enemy, Score.empty, Score.empty, None)
                    val params = BoardComponent.Params(board, Nil, 0)
                    State.Game(info, params)
                  }
                }
              mediator ! PlayerMediator.Event.InGame(ctx.self, gameRef)
              playerInGameBehavior(gameRef)
            case event =>
              ctx.log.error(s"Unexpected $event in playerAwaitGameBehavior")
              Behaviors.stopped
          }
        }
      }

      def playerInGameBehavior(game: Game): Behavior[PlayerActor.Event] =
        Behaviors.receiveMessagePartial {
          playerRespondNameBehavior orElse {
            case PlayerActor.Event.MoveResult(batch) =>
              mediator ! PlayerMediator.Event.MoveResult(batch)
              Behaviors.same
            case PlayerActor.Event.YouWin =>
              transformState(_ => State.YouWin)
              playerAwaitGameBehavior
            case PlayerActor.Event.Draw =>
              transformState(_ => State.Draw)
              playerAwaitGameBehavior
            case PlayerActor.Event.YouLose =>
              transformState(_ => State.YouLose)
              playerAwaitGameBehavior
            case PlayerActor.Event.YourTurn(time) =>
              transformGameState { game =>
                val updatedInfo = game.info.copy(currentPlayer = game.info.you, timeRemaining = Some(time))
                game.copy(info = updatedInfo)
              }
              Behaviors.same
            case PlayerActor.Event.OpponentTurn(time) =>
              transformGameState { game =>
                val updatedInfo = game.info.copy(currentPlayer = game.info.opponent, timeRemaining = Some(time))
                game.copy(info = updatedInfo)
              }
              Behaviors.same
            case PlayerActor.Event.EndOfTurn =>
              transformGameState { game =>
                val updatedInfo = game.info.copy(timeRemaining = None)
                game.copy(info = updatedInfo)
              }
              Behaviors.same
          }
        }

      playerAwaitGameBehavior
    }

    private def transformState(f: State.LoggedInState => State.LoggedInState) =
      access.maybeTransition {
        case appState @ State.LoggedIn(_, loggedIn) =>
          appState.copy(state = f(loggedIn))
        // TODO error state
      }

    private def transformGameState(f: State.Game => State.Game) =
      access.maybeTransition {
        case state @ State.LoggedIn(_, game: State.Game) =>
          state.copy(state = f(game))
        // TODO error state
      }

    private def applyBatch(batch: Batch) = transformGameState {
      case state @ State.Game(_, params) =>
        val updatedParams = params.copy(
          animationNumber = params.animationNumber + 1,
          batch = batch
        )
        state.copy(boardParams = updatedParams)
      // TODO error state
    }
  }

  def setup(access: Access): Future[Extension.Handlers[Future, State, ClientEvent]] = {
    access.sessionId.map { qsi =>
      val id = s"${qsi.deviceId}-${qsi.sessionId}"
      val actors = new Actors(id, access)
      val proxy = actorSystem.spawnAnonymous(actors.Proxy(access))
      Extension.Handlers(
        onDestroy = () => Future.successful(actorSystem.stop(proxy.toClassic)),
        onMessage = clientEvent => {
          Future.successful(proxy ! clientEvent)
        }
      )
    }
  }
}
