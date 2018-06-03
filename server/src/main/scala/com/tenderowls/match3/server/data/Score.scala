package com.tenderowls.match3.server.data

case class Score(data: Map[ColorCell, Int]) extends AnyVal { a =>

  def exists(f: Int => Boolean): Boolean =
    data.values.exists(f)

  def inc(cell: ColorCell): Score = {
    val entry = (cell, a.data.get(cell).fold(0)(_ + 1))
    Score(a.data + entry)
  }

  def +(b: Score): Score = {
    val sum = (a.data.keySet ++ b.data.keySet).map { key =>
      (key, a.data.getOrElse(key, 0) + b.data.getOrElse(key, 0))
    }
    Score(sum.toMap)
  }

  override def toString: String = {
    val xs = data.map { case (k, v) => s"${k.getClass.getSimpleName.dropRight(5).toLowerCase()}=$v" }
    s"Score(${xs.mkString(",")})"
  }
}

object Score {
  val empty = Score(ColorCell.All.zip(Seq.fill(ColorCell.All.size)(0)).toMap)
}
