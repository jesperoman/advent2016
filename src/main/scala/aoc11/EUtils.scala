package aoc11

import aoc11.Device.{Generator, MicroChip}

object EUtils {
  def workingDevices(devices: List[Device]): Boolean = {
    val generators = devices.collect{case g: Generator => g}
    val chips = devices.collect{case m: MicroChip => m}
    generators.isEmpty || chips.isEmpty || chips.forall(c => generators.exists(_.subject == c.subject))
  }

  def possibleFloorMoves(from: Floor, to: List[Floor]): List[(List[Device], List[Floor])] = {
    val floorsUp = to.filter(_.nr > from.nr)
    val floorsDown = to.filter(_.nr < from.nr)

    def loop(devices: List[Device], floors: List[Floor], possibles: List[Floor] = List.empty): List[Floor] = {
      floors match {
        case h :: t if possible(devices, h) => loop(devices, t, h :: possibles)
        case _ => possibles
      }
    }

    val upMoves = from.movableCombinations.map { cs =>
      cs -> loop(cs, floorsUp.sortBy(_.nr))
    }.filter(_._2.nonEmpty)

    val downMoves = from.movableCombinations.filter(_.length == 1).map { cs =>
      cs -> loop(cs, floorsDown.sortBy(_.nr).reverse)
    }.filter(_._2.nonEmpty)

    upMoves ++ downMoves
  }

  def possible(devices: List[Device], to: Floor) =
    workingDevices(to.devices ++ devices)
}
