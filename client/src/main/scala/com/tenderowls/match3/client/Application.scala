package com.tenderowls.match3.client

import akka.actor
import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.adapter._
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.tenderowls.match3._
import com.tenderowls.match3.client.components.BoardComponent
import com.tenderowls.match3.client.components.BoardComponent.Rgb
import com.tenderowls.match3.server.actors.LobbyActor
import com.tenderowls.match3.server.data.{ColorCell, PlayerInfo, Score}
import korolev.akkahttp._
import korolev.execution._
import korolev.server._
import korolev.state.javaSerialization._
import levsha.dsl._
import levsha.dsl.html._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

object Application extends App {

  import State.globalContext._

  private implicit val actorSystem = ActorSystem("match3", defaultExecutionContext = Some(defaultExecutor))
  private implicit val materializer: ActorMaterializer = ActorMaterializer()
  private implicit val askTimeout: Timeout = 1.second
  private implicit val akkaScheduler: actor.Scheduler = actorSystem.scheduler

  final val side = 9
  final val gameTimeout = 30.seconds
  final val maxScore = 10

  val alignItems = StyleDef("align-items")
  val flexDirection = StyleDef("flex-direction")
  val fontStyle = StyleDef("font-style")
  val justifyContent = StyleDef("justify-content")
  val h2 = TagDef("h2")
  val h3 = TagDef("h3")
  val h6 = TagDef("h6")

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

  val playerProxyExtension = new PlayerProxyExtension(lobby, boardRules, gameTimeout, maxScore)

  private val serviceConfig = KorolevServiceConfig[Future, State, ClientEvent](
    stateLoader = StateLoader.default(State.Login: State),
    extensions = List(playerProxyExtension),
    head = { _ =>
      Seq(
        link(href := "static/main.css", rel := "stylesheet", `type` := "text/css"),
        meta(name :="viewport", content := "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
        script(language := "javascript", src := "static/gestures.js")
      )
    },
    render = {
      case State.Login =>
        def onSubmit(access: Access) = {
          for {
            name <- access.valueOf(nameInputId)
            _ <- access.transition(_ => State.LoggedIn(name, State.Lobby))
            _ <- access.publish(ClientEvent.EnterLobby(name))
          } yield ()
        }

        optimize {
          body(
            div(display @= "flex",
              alignItems @= "center",
              flexDirection @= "column",
              h3(textAlign @= "center", "Multiplayer match-three"),
              h6(textAlign @= "center", fontStyle @= "italic",
                "Written in Scala and Korolev by Aleksey Fomkin",
                a(fontStyle @= "italic", display @= "block", href := "https://github.com/tenderowls/match3", "https://github.com/tenderowls/match3")
              ),
              form(clazz := "panel", marginTop @= "15px", display @= "flex",
                input(nameInputId, `type` := "text", placeholder := "Your nickname"),
                button("Enter lobby"),
                event("submit")(onSubmit)
              )
            )
          )
        }
      case State.LoggedIn(name, State.YouWin) =>
        optimize {
          body(
            div(clazz := "panel",
              h2("You win! ❤️"),
              enterLobbyButton(name)
            )
          )
        }

      case State.LoggedIn(name, State.YouLose) =>
        optimize {
          body(
            div(clazz := "panel",
              h2("You lose. \uD83D\uDCA9"),
              enterLobbyButton(name)
            )
          )
        }

      case State.LoggedIn(name, State.Draw) =>
        optimize {
          body(
            div(clazz := "panel",
              h2("Draw"),
              enterLobbyButton(name)
            )
          )
        }

      case State.LoggedIn(_, State.Lobby) =>
        optimize {
          body(
            div(clazz := "panel",
              h2("Looking for opponent..."),
              div(
                button(
                  "Play with bot",
                  event("click")(_.publish(ClientEvent.PlayWithBot))
                )
              )
            )
          )
        }

      case State.LoggedIn(_, State.Game(gameInfo, boardParams)) =>

        def moveIndicator(player: PlayerInfo, score: Score) = {
          val thisMove = gameInfo.currentPlayer == player
          optimize {
            div(
              clazz := (
                if (thisMove) "move-indicator move-indicator__current"
                else "move-indicator"
                ),
              div(
                display @= "flex",
                justifyContent @= "space-between",
                player.name,
                when(thisMove)(div(gameInfo.timeRemaining.map(s => s.toSeconds.toString)))
              ),
              renderScore(score)
            )
          }
        }

        val board = BoardComponent.create(boardParams) { (access, event) =>
          event match {
            case BoardComponent.Event.Move(swap) =>
              access.publish(ClientEvent.MakeMove(swap))
            case BoardComponent.Event.AnimationEnd =>
              access.publish(ClientEvent.SyncAnimation)
            case BoardComponent.Event.AddScore(score) =>
              access.maybeTransition {
                case state@State.LoggedIn(_, game@State.Game(info, _)) if info.currentPlayer == info.you =>
                  state.copy(state = game.copy(info = info.copy(yourScore = info.yourScore + score)))
                case state@State.LoggedIn(_, game@State.Game(info, _)) if info.currentPlayer == info.opponent =>
                  state.copy(state = game.copy(info = info.copy(opponentScore = info.opponentScore + score)))
              }
          }
        }

        optimize {
          body(
            delay(1.second) { access =>
              access.maybeTransition {
                case state@State.LoggedIn(_, game: State.Game) =>
                  state.copy(state = game.copy(info = game.info.copy(timeRemaining = game.info.timeRemaining.map(_ - 1.second))))
              }
            },
            div(clazz := "game",
              moveIndicator(gameInfo.you, gameInfo.yourScore),
              board,
              moveIndicator(gameInfo.opponent, gameInfo.opponentScore)
            )
          )
        }
    }
  )

  private val route = akkaHttpService(serviceConfig).apply(AkkaHttpServerConfig())

  Http().bindAndHandle(route, "0.0.0.0", 8080)

  private def enterLobbyButton(name: String) = {
    def onClick(access: Access) =
      for {
        _ <- access.publish(ClientEvent.EnterLobby(name))
        _ <- access.maybeTransition {
          case state: State.LoggedIn =>
            state.copy(state = State.Lobby)
        }
      } yield ()

    optimize {
      button(
        "Enter lobby",
        event("click")(onClick)
      )
    }
  }

  private def renderScore(score: Score): Node = optimize {
    div(
      score.data.map {
        case (colorCell, count) =>
          val color = BoardComponent.cellToColor(colorCell)
          renderScoreLine(color, 3, count.toDouble / maxScore.toDouble)
      }
    )
  }

  private def renderScoreLine(color: Rgb, h: Int, progress: Double): Node = {
    optimize {
      div(
        clazz := "score",
        height @= s"${h}px",
        backgroundColor @= color.toStringWithAlpha(0.1),
        div(
          clazz := "score-bar",
          width @= s"${Math.min(progress * 100, 100)}%",
          height @= s"${h}px",
          backgroundColor @= color.toString
        )
      )
    }
  }
}