package aoc11

import aoc11.Device.{Generator, MicroChip}

/**
  * Created by jesper on 2016-12-29.
  */
case class State(moves: Int, floors: List[Floor], elevator: Int, prevStates: List[State]) {
  def currentFloor: Floor = floors(elevator)
  def otherFloors: List[Floor] = floors.filterNot(_ == currentFloor)
  def nextStates: List[State] = {
    val nextMoves = EUtils.possibleFloorMoves(currentFloor, otherFloors)
    nextMoves.flatMap {
      case (devices, pf) => pf.map { f =>
        move(devices, elevator, f.nr)
      }
    }
  }

  override def toString = {
    s"State(moves: $moves)\n" +
      floors.sortBy(-_.nr).map{ f =>
        (if (elevator == f.nr) "E" else ".") + f
      }.mkString("\n")
  }

  def hash: String = {
    s"E$elevator" + floors.map { f =>
      val devString = f.devices.map {
        case Generator(s) => s"G${s.take(2)}"
        case MicroChip(s) => s"M${s.take(2)}"
      }.sorted.mkString
      f.nr + devString
    }.mkString
  }

  def move(devices: List[Device], from: Int, to: Int): State = {
    val newFromDevices = floors(from).devices.diff(devices)
    val newToDevices = floors(to).devices.union(devices)
    State(
      moves + Math.abs(from - to),
      floors.updated(from, floors(from).copy(devices = newFromDevices)).updated(to, floors(to).copy(devices = newToDevices)),
      to,
      this::prevStates
    )
  }

}
