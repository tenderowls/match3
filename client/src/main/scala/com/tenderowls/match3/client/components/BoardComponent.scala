package com.tenderowls.match3.client.components

import com.tenderowls.match3.server.data.{ColorCell, Score}
import com.tenderowls.match3.BoardOperation.{Swap, Transition, Update}
import com.tenderowls.match3.Cell.EmptyCell
import com.tenderowls.match3.{Board, BoardOperation, Cell, Point}
import korolev.Component

import scala.concurrent.Future
import scala.concurrent.duration._

import korolev.execution._
import korolev.state.javaSerialization._

import levsha.dsl._
import html._

import scala.util.Random

object BoardComponent {

  /**
    * @param batch Animation batch
    * @param animationNumber Number of animation batch. Increment this number to play new batch
    * @param originalBoard Original board. Component use it to first time.
    *                      After applying animation component will use internal board state
    */
  case class Params(originalBoard: Board, batch: Batch, animationNumber: Int)

  sealed trait Event

  object Event {
    case class Move(swap: Swap) extends Event
    case class AddScore(score: Score) extends Event
    case object AnimationEnd extends Event
  }

  case class Rgb(red: Int, green: Int, blue: Int) {
    def toStringWithAlpha(alpha: Double) = s"rgba($red, $green, $blue, $alpha)"
    override def toString: String = s"rgb($red, $green, $blue)"
  }

  def cellToColor(c: Cell): Rgb = c match {
    case ColorCell.BlueCell   => Rgb(0x00, 0xA3, 0xFF)
    case ColorCell.GreenCell  => Rgb(0x0C, 0xE8, 0x42)
    case ColorCell.RedCell    => Rgb(0xFF, 0x0D, 0x2A)
    case ColorCell.YellowCell => Rgb(0xFF, 0xF7, 0x00)
    case ColorCell.Orange     => Rgb(0xE8, 0x86, 0x0C)
    case ColorCell.Dark       => Rgb(0x33, 0x33, 0x33)
    case _                    => Rgb(0xFF, 0xFF, 0xFF)
  }

  val animationState = AttrDef("animation-state")

  val create: Component[Future, State, Params, Event] = Component[Future, State, Params, Event](State.Static(None, None, 0)) { (context, parameters, state) =>

    import BoardViewConfig.default._
    import context.{Event => DomEvent, _}

    def screenPos(n: Int): Double = {
      n * (cellWidth + cellGap)
    }

    def neighbours(p: Point): List[Point] = {
      List(
        p.left,
        p.right,
        p.top,
        p.bottom,
        p.top.left,
        p.top.right,
        p.bottom.left,
        p.bottom.right
      )
    }

    def calculateMove(ops: List[BoardOperation], point: Point) = ops.collectFirst {
      case Transition(`point`, to) => to
      case Swap(`point`, to)       => to
      case Swap(to, `point`)       => to
    }

    def renderAnimatedCell(ops: List[BoardOperation], point: Point, cell: Cell) = {
      val move = calculateMove(ops, point)
      val isEmptyCell = cell == Cell.EmptyCell
      val updateToNonEmpty = ops.collectFirst {
        case Update(p, c) if p == point && c != EmptyCell => c
      }
      val color = cellToColor(updateToNonEmpty.getOrElse(cell))
      val vsd = viewSide.toDouble
      val wh = {
        if (ops.contains(Update(point, Cell.EmptyCell))) 0
        else if (updateToNonEmpty.nonEmpty) cellWidth
        else if (isEmptyCell) 0
        else cellWidth
      } / vsd * 100
      val xyk = if (wh == 0) cellRadius else 0
      val x = s"${((move.fold(screenPos(point.x))(p => screenPos(p.x)) + xyk) / vsd * 100)}%"
      val y = s"${((move.fold(screenPos(point.y))(p => screenPos(p.y)) + xyk) / vsd * 100)}%"

      optimize {
        div(
          clazz := "circle circle-touchable circle-movable",
          left @= x,
          top @= y,
          width @= (s"${wh}%"),
          height @= (s"${wh}%"),
          backgroundColor @= color.toStringWithAlpha(1.0)
        )
      }
    }

    def renderAnimatedBoard(board: Board,
                            ops: List[BoardOperation])
                           (onAnimationEnd: Access => Future[Unit]) = optimize {
      div(
        clazz := "board",
        // Fake circle need to track transition end
        div(
          animationState := "animated",
          clazz := "circle circle-movable",
          left @= s"${(Random.nextInt(49) + 51)}%",
          top @= "0px",
          width @= "20px",
          height @= "20px",
          backgroundColor @= "#FFFFFF",
          event("transitionend")(onAnimationEnd)
        ),
        board.data.map {
          case (point, cell) =>
            renderAnimatedCell(ops, point, cell)
        }
      )
    }

    def renderStaticBoard(board: Board,
                          selectedCellOpt: Option[Point],
                          circleClass: Option[String],
                          effects: Seq[Effect] = Nil)
                         (cellClick: Point => Option[Access => Future[Unit]],
                          onAnimationEnd: Option[Access => Future[Unit]] = None) = {

      def renderStaticCell(point: Point, cell: Cell) = {
        val eventHandler = cellClick(point)
        val vsd = viewSide.toDouble
        val wh = {
          if (cell == Cell.EmptyCell) 0
          else if (selectedCellOpt.contains(point)) cellWidth + cellGap
          else cellWidth
        } / vsd * 100
        val xyk = if (wh == 0) cellRadius else 0
        val x = s"${((screenPos(point.x) + xyk) / vsd * 100)}%"
        val y = s"${((screenPos(point.y) + xyk) / vsd * 100)}%"
        val opacity = selectedCellOpt.fold(1d) { selectedCell =>
          if (point == selectedCell) 1d
          else if (neighbours(selectedCell).contains(point)) 1d
          else 0.5d
        }

        optimize {
          div(
            left @= x,
            top @= y,
            backgroundColor @= cellToColor(cell).toStringWithAlpha(opacity),
            width @= (s"${wh}%"),
            height @= (s"${wh}%"),
            clazz := "circle " + circleClass.getOrElse(""),
            eventHandler.map(f => event("mousedown")(f)),
            eventHandler.map(f => event("touchend")(f))
          )
        }
      }

      optimize {
        div(
          clazz := "board",
          // Fake circle need to track transition end
          div(
            animationState := "static",
            clazz := "circle circle-movable",
            left @= s"${Random.nextInt(50)}%",
            top @= "0px",
            width @= "20px",
            height @= "20px",
            backgroundColor @= "#FFFFFF",
            onAnimationEnd.map(event("transitionend")(_))
          ),
          board.data.map {
            case (point, cell) =>
              renderStaticCell(point, cell)
          },
          effects
        )
      }
    }

    (parameters, state) match {
      case (Params(origBoard, _, an), State.Static(boardOpt, selectedCellOpt, lan)) if an <= lan =>
        val board = boardOpt.getOrElse(origBoard)
        renderStaticBoard(board, selectedCellOpt, Some("circle-touchable")) { p2 =>
          Some {
            { access =>
              val swapOpt = selectedCellOpt.flatMap { p1 =>
                if (neighbours(p1).contains(p2)) Some(Swap(p1, p2))
                else None
              }
              swapOpt match {
                case Some(swap) => access.publish(Event.Move(swap))
                case None if selectedCellOpt.contains(p2) => access.maybeTransition {
                  case s: State.Static => s.copy(selectedCell = None)
                }
                case None => access.maybeTransition {
                  case s: State.Static => s.copy(selectedCell = Some(p2))
                }
              }
            }
          }
        }
      case (Params(origBoard, ops :: batch, an), State.Static(boardOpt, _, lan)) if an > lan =>
        val board = boardOpt.getOrElse(origBoard)
        // Enter animation
        val newBoard = board.applyOperations(ops)
        renderAnimatedBoard(board, ops) { access =>
          access.sessionId.flatMap { qsid =>
            println(s"${qsid.deviceId.take(4)}: transition end (remove or add circles ${batch.length}")
            access.transition(_ => State.AnimationEnd(an, newBoard, batch))
          }
        }
      case (_, State.AnimationStart(an, board, ops :: batch)) =>
        // Do animation
        renderAnimatedBoard(board, ops) { access =>
          val newBoard = board.applyOperations(ops)
          val score = ops.foldLeft(Score.empty) {
            case (total, BoardOperation.Update(point, Cell.EmptyCell)) =>
              board.get(point).fold(total) {
                case EmptyCell => total
                case cell: ColorCell => total.inc(cell)
              }
            case (total, _) => total
          }
          for {
            qsid <- access.sessionId
            _ = println(s"${qsid.deviceId.take(4)}: transition end (remove or add circles ${batch.length}")
            _ <- if (score.sum > 0) access.publish(Event.AddScore(score)) else Future.unit
            _ <- access.transition(_ => State.AnimationEnd(an, newBoard, batch))
          } yield ()
        }
      case (_, State.AnimationEnd(an, board, batch)) =>
        //        // End animation
        //        val effect = delay(animationDelay) { access =>
        //          if (batch.isEmpty) {
        //            println("animation end")
        //            for {
        //              _ <- access.transition(_ => State.Static(Some(board), None, an))
        //              _ <- access.publish(Event.AnimationEnd)
        //            } yield ()
        //          } else {
        //            access.transition(_ => State.AnimationStart(an, board, batch))
        //          }
        //        }
        //        renderStaticBoard(board, None, None, Seq(effect))(_ => None)
        renderStaticBoard(board, None, None, Nil)(
          cellClick = _ => None,
          onAnimationEnd = Some {
            access: Access => {
              if (batch.isEmpty) {
                for {
                  _ <- access.transition(_ => State.Static(Some(board), None, an))
                  _ <- access.publish(Event.AnimationEnd)
                } yield ()
              } else {
                access.transition(_ => State.AnimationStart(an, board, batch))
              }
            }
          }
        )
    }
  }

  type PositionPolicy = (Point, Cell) => (Double, Double)
  type OpacityPolicy = (Point, Cell) => Double
  type RadiusPolicy = (Point, Cell) => Double
  type Batch = List[List[BoardOperation]]

  case class BoardViewConfig(
                              side: Int = 9,
                              cellRadius: Int = 15,
                              cellGap: Int = 4,
                              animationDuration: FiniteDuration = 200.millis,
                              animationDelay: FiniteDuration = 100.millis
                            ) {
    val cellWidth: Int = cellRadius * 2
    val viewSide: Int = side * (cellWidth + cellGap)
  }

  object BoardViewConfig {
    val default = BoardViewConfig()
  }

  sealed trait State

  object State {
    case class Static(boardOpt: Option[Board], selectedCell: Option[Point], lastAnimationNumber: Int) extends State
    case class AnimationStart(animationNumber: Int, board: Board, operations: Batch) extends State
    case class AnimationEnd(animationNumber: Int, board: Board, operations: Batch) extends State
  }
}
