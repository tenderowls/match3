package com.tenderowls.match3

import Direction._

case class Point(x: Int, y: Int) { point =>

  def left = Point(point.x - 1, point.y)

  def left(x: Int) = Point(point.x - x, point.y)

  def right = Point(point.x + 1, point.y)

  def right(x: Int) = Point(point.x + x, point.y)

  def top = Point(point.x, point.y - 1)

  def top(x: Int) = Point(point.x, point.y - x)

  def bottom = Point(point.x, point.y + 1)

  def bottom(x: Int) = Point(point.x, point.y + x)

  def direction(to: Point): Direction = {
    if (point.x == to.x) Vertical
    else Horizontal
  }
}
