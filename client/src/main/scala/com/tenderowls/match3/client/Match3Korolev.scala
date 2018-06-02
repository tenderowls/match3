package com.tenderowls.match3.client

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.typed.ActorRef
import akka.typed.scaladsl.adapter._
import com.tenderowls.match3._
import com.tenderowls.match3.client.State.GameInfo
import com.tenderowls.match3.client.components.BoardComponent
import com.tenderowls.match3.client.components.BoardComponent.Rgb
import com.tenderowls.match3.server.actors.{GameActor, LobbyActor, PlayerActor}
import com.tenderowls.match3.server.data.{ColorCell, PlayerInfo, Score}
import korolev.Context
import korolev.blazeServer._
import korolev.execution._
import korolev.server.KorolevServiceConfig.Env
import korolev.server._
import korolev.state.javaSerialization._
import levsha.Document
import org.http4s.blaze.http.HttpService

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

object Match3Korolev extends KorolevBlazeServer {

  import State.globalContext._
  import State.globalContext.symbolDsl._

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val materializer: ActorMaterializer = ActorMaterializer()

  final val side = 9
  final val timeout = 30.seconds
  final val animationDuration = 350.millis
  final val maxScore = 10

  implicit val boardRules: Rules = new Rules {
    def randomValue: Cell = Random.nextInt(6) match {
      case 0 => ColorCell.RedCell
      case 1 => ColorCell.GreenCell
      case 2 => ColorCell.BlueCell
      case 3 => ColorCell.YellowCell
      case 4 => ColorCell.GrayCell
      case 5 => ColorCell.CyanCell
    }
    val width: Int = side
    val height: Int = side
  }

  private val lobby = actorSystem.spawn(LobbyActor(timeout, animationDuration, boardRules, maxScore), s"lobby")
  private val nameInputId = elementId

  val service: HttpService = blazeService[Future, State, ClientEvent] from KorolevServiceConfig[Future, State, ClientEvent](
    stateStorage = StateStorage.default(State.Login),
    envConfigurator = { (deviceId, sessionId, applyTransition) =>

      val id = s"$deviceId-$sessionId"
      var player: ActorRef[PlayerActor.Event] = null
      var game: ActorRef[BoardOperation.Swap] = null

      def stopPlayer(): Unit = if (player != null) {
        actorSystem.stop(player.toUntyped)
        player = null
      }

      Env(
        onDestroy = { () =>
          stopPlayer()
        },
        onMessage = {
          case ClientEvent.PlayWithBot =>
            val board = BoardGenerator.square()(boardRules)
            val bot = actorSystem.spawn(PlayerActor.bot("bot"), s"bot-${LobbyActor.mkId}")
            val gameBehavior = GameActor(bot, player, board, timeout, animationDuration, boardRules, maxScore)
            lobby ! LobbyActor.Event.Leave(player)
            actorSystem.spawn(gameBehavior, s"game-${LobbyActor.mkId}")
          case ClientEvent.MakeMove(swap) =>
            if (game != null)
              game ! swap
          case ClientEvent.EnterLobby(name) =>
            player = {
              val behavior = PlayerActor.localPlayer(name) {
                case PlayerActor.Event.GameStarted(board, gameRef, opponent) =>
                  game = gameRef
                  applyTransition { _ =>
                    val you = PlayerInfo(name)
                    val enemy = PlayerInfo("?")
                    val info = GameInfo(enemy, you, enemy, Score.empty, Score.empty, None)
                    val params = BoardComponent.Params(board, Nil, 0)
                    State.Game(info, params)
                  }
                case PlayerActor.Event.YourTurn(time) =>
                  applyTransition {
                    case game @ State.Game(info, _) =>
                      game.copy(info.copy(currentPlayer = info.you, timeRemaining = Some(time)))
                  }

                case PlayerActor.Event.OpponentTurn(time) =>
                  applyTransition {
                    case game @ State.Game(info, _) =>
                      game.copy(info.copy(currentPlayer = info.opponent, timeRemaining = Some(time)))
                  }

                case PlayerActor.Event.EndOfTurn =>
                  applyTransition {
                    case game @ State.Game(info, _) =>
                      game.copy(info.copy(timeRemaining = None))
                  }

                case PlayerActor.Event.MoveResult(batch) =>
                  println(s"! move result $batch")
                  applyTransition {
                    case State.Game(info, params) =>
                      println(s"! transition matched")
                      val updatedParams = params.copy(
                        animationNumber = params.animationNumber + 1,
                        batch = batch
                      )
                      State.Game(info, updatedParams)
                  }

                case PlayerActor.Event.YouWin =>
                  stopPlayer()
                  applyTransition(_ => State.YouWin)

                case PlayerActor.Event.YouLose =>
                  stopPlayer()
                  applyTransition(_ => State.YouLose)

                case _ => ()
              }
              actorSystem.spawn(behavior, s"player-$id")
            }
            lobby ! LobbyActor.Event.Enter(player)
        }
      )
    },
    head = Seq(
      'link('href /= "main.css", 'rel /= "stylesheet", 'type /= "text/css")
    ),
    render = {
      case State.Login =>
        'body(
          'form(
            'input(nameInputId, 'type /= "text"),
            'button("Enter"),
            event('submit) { access =>
              access.property(nameInputId, 'value).flatMap { name =>
                access.transition(_ => State.Lobby(name, lookingForOpponent = false))
              }
            }
          )
        )
      case State.YouWin =>
        'body("You win", enterLobbyButton("player-unknown")) // TODO

      case State.YouLose =>
        'body("You lose", enterLobbyButton("player-unknown")) // TODO

      case State.Lobby(_, true) =>
        'body(
          "Looking for opponent...",
          'button("Play with bot", event('click)(_.publish(ClientEvent.PlayWithBot)))
        )
      case State.Lobby(name, false) =>
        'body(enterLobbyButton(name))
      case State.Game(gameInfo, boardParams) =>
        'body(
          'div(
            gameInfo.timeRemaining.toString(),
            delay(1.second) { access =>
              access.transition {
                case game: State.Game =>
                  game.copy(info = game.info.copy(timeRemaining = game.info.timeRemaining.map(_ - 1.second)))
                case s => s
              }
            }
          ),
          'div(
            if (gameInfo.currentPlayer == gameInfo.you) 'border @= "1px solid black" else void,
            gameInfo.you.toString,
            renderScore(gameInfo.yourScore)
          ),
          BoardComponent.create(boardParams) { (access, event) =>
            event match {
              case BoardComponent.Event.Move(swap) =>
                access.publish(ClientEvent.MakeMove(swap))
              case BoardComponent.Event.AddScore(score) =>
                access.transition {
                  case game @ State.Game(info, _) if info.currentPlayer == info.you =>
                    game.copy(info = info.copy(yourScore = info.yourScore + score))
                  case game @ State.Game(info, _) if info.currentPlayer == info.opponent =>
                    game.copy(info = info.copy(opponentScore = info.opponentScore + score))
                  case game =>
                    game
                }
            }
          },
          'div(
            if (gameInfo.currentPlayer == gameInfo.opponent) 'border @= "1px solid black" else void,
            gameInfo.opponent.toString,
            renderScore(gameInfo.opponentScore)
          )
        )
    },
    serverRouter = ServerRouter.empty[Future, State]
  )

  private def enterLobbyButton(name: String) = {
    'button(
      "Enter lobby",
      event('click) { access =>
        access.publish(ClientEvent.EnterLobby(name)).flatMap { _ =>
          access.transition {
            case lobby: State.Lobby =>
              lobby.copy(lookingForOpponent = true)
          }
        }
      }
    )
  }

  private def renderScore(score: Score): Document.Node[Context.Effect[Future, State, ClientEvent]] = {
    'div(
      score.data.map {
        case (colorCell, count) =>
          val color = BoardComponent.cellToColor(colorCell)
          renderScoreLine(color, 500, 10, count.toDouble / maxScore.toDouble)
      }
    )
  }

  private def renderScoreLine(color: Rgb, width: Int, height: Int, progress: Double): Document.Node[Context.Effect[Future, State, ClientEvent]] = {
    'div(
      'class /= "score",
      'width @= width,
      'height @= height,
      'backgroundColor @= color.toStringWithAlpha(0.1),
      'div(
        'class /= "score-bar",
        'width @= Math.min(width, width * progress),
        'height @= height,
        'backgroundColor @= color.toString
      )
    )
  }
}


