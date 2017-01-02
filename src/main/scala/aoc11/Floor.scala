package aoc11

import aoc11.Device.{Generator, MicroChip}

case class Floor(nr: Int, devices: List[Device] = List.empty) {
  val generators = devices.collect{
    case g: Generator => g
  }
  val chips = devices.collect{
    case c: MicroChip => c
  }

  def works: Boolean = {
    (generators, chips) match {
      case (Nil, c) => true
      case (gs, Nil) => true
      case (gs, cs) => cs.forall(c => gs.exists(_.subject == c.subject))
    }
  }

  def movableCombinations: List[List[Device]] = {
    (devices.combinations(2) ++ devices.combinations(1)).filter { x =>
      EUtils.workingDevices(x) && EUtils.workingDevices(devices.diff(x))
    }.toList
  }

  override def toString = {
    s"$nr:..${devices.mkString("..").padTo(40, ".").mkString}"
  }
}
