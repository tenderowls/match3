package com.tenderowls.match3.server.data

import com.tenderowls.match3.Cell

sealed trait ColorCell extends Cell

object ColorCell {
  case object RedCell extends ColorCell
  case object GreenCell extends ColorCell
  case object BlueCell extends ColorCell
  case object YellowCell extends ColorCell
  case object Orange extends ColorCell
  case object Dark extends ColorCell

  final val All = Set(RedCell, GreenCell, BlueCell, YellowCell, Orange, Dark)
}
