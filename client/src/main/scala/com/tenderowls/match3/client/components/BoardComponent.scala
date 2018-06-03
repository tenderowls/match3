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
import levsha.Document.Attr
import levsha.XmlNs

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
  }

  case class Rgb(red: Int, green: Int, blue: Int) {
    def toStringWithAlpha(alpha: Double) = s"rgba($red, $green, $blue, $alpha)"
    override def toString: String = s"rgb($red, $green, $blue)"
  }

  def cellToColor(c: Cell) = c match {
    case ColorCell.RedCell    => Rgb(0xee, 0x00, 0x00)
    case ColorCell.GreenCell  => Rgb(0x00, 0xEE, 0x00)
    case ColorCell.BlueCell   => Rgb(0x00, 0x00, 0xEE)
    case ColorCell.YellowCell => Rgb(0xEE, 0xEE, 0x00)
    case ColorCell.GrayCell   => Rgb(0xCC, 0xCC, 0xCC)
    case ColorCell.CyanCell   => Rgb(0x00, 0xFF, 0xFF)
    case _                    => Rgb(0xFF, 0xFF, 0xFF)
  }

  val create = Component[Future, State, Params, Event](State.Static(None, None, 0)) { (context, parameters, state) =>

    import BoardViewConfig.default._
    import context.{Event => DomEvent, _}
    import symbolDsl._

    def screenPos(n: Int): Double = {
      cellRadius + n * (cellWidth + cellGap)
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

    def renderAnimatedBoard(board: Board, ops: List[BoardOperation])(delay: Effect) = {
      def calculateMove(point: Point) = ops.collectFirst {
        case Transition(`point`, to) => to
        case Swap(`point`, to)       => to
        case Swap(to, `point`)       => to
      }
      ns.svg('svg)(
        Attr(_.setAttr(XmlNs.html, "viewBox", s"0 0 ${viewSide.toString} ${viewSide.toString}")),
        'class /= "board",
        board.data.map {
          case (point, cell) =>
            val move = calculateMove(point)
            val x = move.fold(screenPos(point.x))(p => screenPos(p.x))
            val y = move.fold(screenPos(point.y))(p => screenPos(p.y))
            val isEmptyCell = cell == Cell.EmptyCell
            val updateToNonEmpty = ops.collectFirst {
              case Update(p, c) if p == point && c != EmptyCell => c
            }
            val color = cellToColor(updateToNonEmpty.getOrElse(cell))
            val radius = {
              if (ops.contains(Update(point, Cell.EmptyCell))) 0
              else if (updateToNonEmpty.nonEmpty) cellRadius
              else if (isEmptyCell) 0
              else cellRadius
            }
            ns.svg('circle)(
              'class /= "circle-touchable circle-movable",
              'cx /= x.toString,
              'cy /= y.toString,
              'r /= radius.toString,
              'fill /= color.toString,
              'fillOpacity /= "1"
            )
        },
        delay
      )
    }

    def renderStaticBoard(board: Board,
                          selectedCellOpt: Option[Point],
                          circleClass: Option[String],
                          effects: Seq[Effect] = Nil)(cellClick: Point => Option[DomEvent]) = {
      ns.svg('svg)(
        Attr(_.setAttr(XmlNs.html, "viewBox", s"0 0 ${viewSide.toString} ${viewSide.toString}")),
        'class /= "board",
        board.data.map {
          case (point, cell) =>
            val x = screenPos(point.x)
            val y = screenPos(point.y)
            val radius = {
              if (cell == Cell.EmptyCell) 0
              else if (selectedCellOpt.contains(point)) cellRadius + cellGap / 2
              else cellRadius
            }
            val opacity = selectedCellOpt.fold(1d) { selectedCell =>
              if (point == selectedCell) 1d
              else if (neighbours(selectedCell).contains(point)) 1d
              else 0.09d
            }
            ns.svg('circle)(
              circleClass.map(x => 'class /= x),
              'cx /= x.toString,
              'cy /= y.toString,
              'r /= radius.toString,
              'fill /= cellToColor(cell).toString,
              'fillOpacity /= opacity.toString,
              cellClick(point)
            )
        },
        effects
      )
    }

    (parameters, state) match {
      case (Params(origBoard, _, an), State.Static(boardOpt, selectedCellOpt, lan)) if an <= lan =>
        val board = boardOpt.getOrElse(origBoard)
        renderStaticBoard(board, selectedCellOpt, Some("circle-touchable")) { p2 =>
          Some {
            event('mousedown) { access =>
              val swapOpt = selectedCellOpt.flatMap { p1 =>
                if (neighbours(p1).contains(p2)) Some(Swap(p1, p2))
                else None
              }
              swapOpt match {
                case Some(swap) => access.publish(Event.Move(swap))
                case None if selectedCellOpt.contains(p2) => access.transition {
                  case s: State.Static => s.copy(selectedCell = None)
                  case s => s
                }
                case None => access.transition {
                  case s: State.Static => s.copy(selectedCell = Some(p2))
                  case s => s
                }
              }
            }
          }
        }
      case (Params(origBoard, ops :: batch, an), State.Static(boardOpt, _, lan)) if an > lan =>
        val board = boardOpt.getOrElse(origBoard)
        // Enter animation
        val newBoard = board.applyOperations(ops)
        renderAnimatedBoard(board, ops) {
          delay(animationDuration) { access =>
            access.transition(_ => State.AnimationEnd(an, newBoard, batch))
          }
        }
      case (_, State.AnimationStart(an, board, ops :: batch)) =>
        // Do animation
        renderAnimatedBoard(board, ops) {
          delay(animationDuration) { access =>
            val newBoard = board.applyOperations(ops)
            val score = ops.foldLeft(Score.empty) {
              case (total, BoardOperation.Update(point, Cell.EmptyCell)) =>
                board.get(point).fold(total) {
                  case EmptyCell => total
                  case cell: ColorCell => total.inc(cell)
                }
              case (total, _) => total
            }
            access.publish(Event.AddScore(score)).flatMap { _ =>
              access.transition(_ => State.AnimationEnd(an, newBoard, batch))
            }
          }
        }
      case (_, State.AnimationEnd(an, board, batch)) =>
        // End animation
        val effect = delay(animationDelay) { access =>
          access.transition {
            case _ if batch.isEmpty => State.Static(Some(board), None, an)
            case _                  => State.AnimationStart(an, board, batch)
          }
        }
        renderStaticBoard(board, None, None, Seq(effect))(_ => None)
    }
  }

  type PositionPolicy = (Point, Cell) => (Double, Double)
  type OpacityPolicy = (Point, Cell) => Double
  type RadiusPolicy = (Point, Cell) => Double
  type Batch = List[List[BoardOperation]]

  case class BoardViewConfig(
      side: Int = 9,
      cellRadius: Int = 15,
      cellGap: Int = 2,
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
