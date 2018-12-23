package com.tenderowls.match3.client

import akka.actor
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import com.tenderowls.match3._
import com.tenderowls.match3.client.State.GameInfo
import com.tenderowls.match3.client.components.BoardComponent
import com.tenderowls.match3.client.components.BoardComponent.Rgb
import com.tenderowls.match3.server.actors.{GameActor, LobbyActor, PlayerActor}
import com.tenderowls.match3.server.data.{ColorCell, PlayerInfo, Score}
import korolev.Context
import korolev.execution._
import korolev.server._
import korolev.akkahttp._
import korolev.state.EnvConfigurator
import korolev.state.javaSerialization._
import levsha.Document

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

object Application extends App {

  import State.globalContext._
  import State.globalContext.symbolDsl._

  private implicit val actorSystem: ActorSystem = ActorSystem("match3", defaultExecutionContext = Some(defaultExecutor))
  private implicit val materializer: ActorMaterializer = ActorMaterializer()
  private implicit val askTimeout: Timeout = 1.second
  private implicit val akkaScheduler: actor.Scheduler = actorSystem.scheduler

  final val side = 9
  final val gameTimeout = 30.seconds
  final val maxScore = 10

  implicit val boardRules: Rules = new Rules {
    def randomValue: Cell = Random.nextInt(6) match {
      case 0 => ColorCell.RedCell
      case 1 => ColorCell.GreenCell
      case 2 => ColorCell.BlueCell
      case 3 => ColorCell.YellowCell
      case 4 => ColorCell.Orange
      case 5 => ColorCell.Dark
    }
    val width: Int = side
    val height: Int = side
  }

  private val lobby = actorSystem.spawn(LobbyActor(gameTimeout, boardRules, maxScore), s"lobby")
  private val nameInputId = elementId()

  private val serviceConfig = KorolevServiceConfig[Future, State, ClientEvent](
    stateStorage = StateStorage.default(State.Login),
    envConfigurator = EnvConfigurator { access =>
      access.sessionId.map { qsi =>
        val id = s"${qsi.deviceId}-${qsi.id}"
        var player: ActorRef[PlayerActor.Event] = null
        var game: ActorRef[GameActor.Event] = null
        var animated = false
        val pendingMoveResult = mutable.Queue.empty[PlayerActor.Event.MoveResult]

        def stopPlayer(): Unit = if (player != null) {
          actorSystem.stop(player.toUntyped)
          player = null
        }

        EnvConfigurator.Env(
          onDestroy = { () =>
            stopPlayer()
            Future.unit
          },
          onMessage = {
            case ClientEvent.PlayWithBot =>
              val board = BoardGenerator.square()(boardRules)
              val bot = actorSystem.spawn(PlayerActor.bot("bot"), s"bot-${LobbyActor.mkId}")
              val gameBehavior = GameActor(player, bot, board, gameTimeout, boardRules, maxScore)
              lobby ! LobbyActor.Event.Leave(player)
              game = actorSystem.spawn(gameBehavior, s"game-${LobbyActor.mkId}")
              Future.unit
            case ClientEvent.MakeMove(swap) =>
              if (game != null)
                game ! GameActor.Event.MakeMove(player, swap)
              Future.unit
            case ClientEvent.EnterLobby(name) =>
              player = {
                val behavior = PlayerActor.localPlayer(name) {
                  case PlayerActor.Event.GameStarted(yourTurn, board, gameRef, opponent) =>
                    game = gameRef
                    opponent.?[String](PlayerActor.Event.WhatsYourName) flatMap { opponentName =>
                      access.maybeTransition {
                        case state: State.LoggedIn =>
                          val you = PlayerInfo(name)
                          val enemy = PlayerInfo(opponentName)
                          val currentPlayer = if (yourTurn) you else enemy
                          val info = GameInfo(currentPlayer, you, enemy, Score.empty, Score.empty, None)
                          val params = BoardComponent.Params(board, Nil, 0)
                          state.copy(state = State.Game(info, params))
                      }
                    }
                  case PlayerActor.Event.YourTurn(time) =>
                    access.maybeTransition {
                      case state @ State.LoggedIn(_, game @ State.Game(info, _)) =>
                        state.copy(state = game.copy(info.copy(currentPlayer = info.you, timeRemaining = Some(time))))
                    }

                  case PlayerActor.Event.OpponentTurn(time) =>
                    access.maybeTransition {
                      case state@State.LoggedIn(_, game@State.Game(info, _)) =>
                        state.copy(state = game.copy(info.copy(currentPlayer = info.opponent, timeRemaining = Some(time))))
                    }

                  case PlayerActor.Event.EndOfTurn =>
                    access.maybeTransition {
                      case state @ State.LoggedIn(_, game @ State.Game(info, _)) =>
                        state.copy(state = game.copy(info.copy(timeRemaining = None)))
                    }

                  case mr @ PlayerActor.Event.MoveResult(batch) =>
                    if (animated) {
                      pendingMoveResult.enqueue(mr)
                    } else {
                      animated = true
                      access.maybeTransition {
                        case state @ State.LoggedIn(_, State.Game(info, params)) =>
                          val updatedParams = params.copy(
                            animationNumber = params.animationNumber + 1,
                            batch = batch
                          )
                          state.copy(state = State.Game(info, updatedParams))
                      }
                    }

                  case PlayerActor.Event.YouWin =>
                    stopPlayer()
                    access.maybeTransition {
                      case state: State.LoggedIn =>
                        state.copy(state = State.YouWin)
                    }

                  case PlayerActor.Event.YouLose =>
                    stopPlayer()
                    access.maybeTransition {
                      case state: State.LoggedIn =>
                        state.copy(state = State.YouLose)
                    }
                  case score @ PlayerActor.Event.CurrentScore(yours, opponents) =>
                    println(score)
//                    access.maybeTransition {
//                      case state @ State.LoggedIn(_, State.Game(info, params)) =>
//                        val updatedInfo = info.copy(yourScore = yours, opponentScore = opponents)
//                        state.copy(state = State.Game(updatedInfo, params))
//                    }

                  case _ => ()
                }
                actorSystem.spawn(behavior, s"player-$id")
              }
              lobby ! LobbyActor.Event.Enter(player)
              Future.unit
            case ClientEvent.SyncAnimation =>
              if (pendingMoveResult.nonEmpty) {
                val PlayerActor.Event.MoveResult(batch) = pendingMoveResult.dequeue
                // TODO remove copy pase
                access.maybeTransition {
                  case state @ State.LoggedIn(_, State.Game(info, params)) =>
                    val updatedParams = params.copy(
                      animationNumber = params.animationNumber + 1,
                      batch = batch
                    )
                    state.copy(state = State.Game(info, updatedParams))
                }
              } else {
                animated = false
              }
              game ! GameActor.Event.AnimationFinished
              Future.unit
          }
        )
      }
    },
    head = Seq(
      'link('href /= "main.css", 'rel /= "stylesheet", 'type /= "text/css"),
      'meta('name /="viewport", 'content /= "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")
    ),
    render = {
      case State.Login =>
        'body(
          'div('display @= "flex",
               'alignItems @= "center",
               'flexDirection @= "column",
            'h3('textAlign @= "center", "Multiplayer match-three"),
            'h6('textAlign @= "center", 'fontStyle @= "italic",// 'maxWidth @= 400,
              "Written in Scala and Korolev by Aleksey Fomkin",
              'a('fontStyle @= "italic", 'display @= "block", 'href /= "https://github.com/tenderowls/match3", "https://github.com/tenderowls/match3")
            ),
            'form('class /= "panel", 'marginTop @= 15, 'display @= "flex",
              'input(nameInputId, 'type /= "text", 'placeholder /= "Your nickname"),
              'button("Enter lobby"),
              event('submit) { access =>
                for {
                  name <- access.property(nameInputId, 'value)
                  _ <- access.transition(_ => State.LoggedIn(name, State.Lobby))
                  _ <- access.publish(ClientEvent.EnterLobby(name))
                } yield ()
              }
            )
          )
        )
      case State.LoggedIn(name, State.YouWin) =>
        'body(
          'div('class /= "panel",
            'h2("You win! ❤️"),
            enterLobbyButton(name)
          )
        )

      case State.LoggedIn(name, State.YouLose) =>
        'body(
          'div('class /= "panel",
            'h2("You lose. \uD83D\uDCA9"),
            enterLobbyButton(name)
          )
        )

      case State.LoggedIn(_, State.Lobby) =>
        'body(
          'div('class /= "panel",
            'h2("Looking for opponent..."),
            'div(
              'button(
                "Play with bot",
                event('click)(_.publish(ClientEvent.PlayWithBot))
              )
            )
          )
        )

      case State.LoggedIn(_, State.Game(gameInfo, boardParams)) =>

        def moveIndicator(player: PlayerInfo, score: Score) = {
          val thisMove = gameInfo.currentPlayer == player
          'div(
            if (thisMove) 'class /= "move-indicator move-indicator__current"
            else 'class /= "move-indicator",
            'div(
              'display @= "flex",
              'justifyContent @= "space-between",
              player.name,
              if (thisMove) gameInfo.timeRemaining.map(s => 'div(s.toSeconds.toString)) else void
            ),
            renderScore(score)
          )
        }

        'body(
          delay(1.second) { access =>
            access.maybeTransition {
              case state @ State.LoggedIn(_, game: State.Game) =>
                state.copy(state = game.copy(info = game.info.copy(timeRemaining = game.info.timeRemaining.map(_ - 1.second))))
            }
          },
          'div('class /= "game",
            moveIndicator(gameInfo.you, gameInfo.yourScore),
            BoardComponent.create(boardParams) { (access, event) =>
              event match {
                case BoardComponent.Event.Move(swap) =>
                  access.publish(ClientEvent.MakeMove(swap))
                case BoardComponent.Event.AnimationEnd =>
                  access.publish(ClientEvent.SyncAnimation)
                case BoardComponent.Event.AddScore(score) =>
                  access.maybeTransition {
                    case state @ State.LoggedIn(_, game @ State.Game(info, _)) if info.currentPlayer == info.you =>
                      state.copy(state = game.copy(info = info.copy(yourScore = info.yourScore + score)))
                    case state @ State.LoggedIn(_, game @ State.Game(info, _)) if info.currentPlayer == info.opponent =>
                      state.copy(state = game.copy(info = info.copy(opponentScore = info.opponentScore + score)))
                  }
              }
            },
            moveIndicator(gameInfo.opponent, gameInfo.opponentScore)
          )
        )
    },
    router = emptyRouter
  )

  private val route = akkaHttpService(serviceConfig).apply(AkkaHttpServerConfig())

  Http().bindAndHandle(route, "0.0.0.0", 8080)

  private def enterLobbyButton(name: String) = {
    'button(
      "Enter lobby",
      event('click) { access =>
        for {
          _ <- access.publish(ClientEvent.EnterLobby(name))
          _ <- access.maybeTransition {
            case state: State.LoggedIn =>
              state.copy(state = State.Lobby)
          }
        } yield ()
      }
    )
  }

  private def renderScore(score: Score): Document.Node[Context.Effect[Future, State, ClientEvent]] = {
    'div(
      score.data.map {
        case (colorCell, count) =>
          val color = BoardComponent.cellToColor(colorCell)
          renderScoreLine(color, 10, count.toDouble / maxScore.toDouble)
      }
    )
  }

  private def renderScoreLine(color: Rgb, height: Int, progress: Double): Document.Node[Context.Effect[Future, State, ClientEvent]] = {
    'div(
      'class /= "score",
      'height @= height,
      'backgroundColor @= color.toStringWithAlpha(0.1),
      'div(
        'class /= "score-bar",
        'width @= s"${Math.min(progress * 100, 100)}%",
        'height @= height,
        'backgroundColor @= color.toString
      )
    )
  }
}


