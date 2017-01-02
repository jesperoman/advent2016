package aoc11

import aoc11.Device.{Generator, MicroChip}
import util.Utils

import scala.annotation.tailrec
import scala.util.matching.Regex.Match

object Eleventh extends App {
  val inputReal = Utils.load("/input11.txt", getClass)

  val inputSecond = """The first floor contains a elerium generator, a elerium-compatible microchip, a dilithium generator, a dilithium-compatible microchip, a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
                      |The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
                      |The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
                      |The fourth floor contains nothing relevant.""".stripMargin.split("\n").toList.map(_.trim)

  val inputExample =
    """The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
      |The second floor contains a hydrogen generator.
      |The third floor contains a lithium generator.
      |The fourth floor contains nothing relevant.""".stripMargin.split("\n").toList.map(_.trim)

  val floorPattern = "The (\\w+) floor contains (.*)".r
  val itemPattern = "a ((\\w+)( generator|-compatible microchip))".r

  val start = inputSecond.map{
    case floorPattern(f, c) =>
      itemPattern.findAllMatchIn(c).toList.foldLeft(Floor(fromString(f))) { (floor, m) =>
        fromMatch(m, floor)
      }
  }

  val startState = State(0, start, 0, List.empty)

  val doneHash = {
    val topFloor = startState.floors.maxBy(_.nr).nr
    val allDevices = startState.floors.flatMap(_.devices)
    State(0, startState.floors.map {
      case f if f.nr == topFloor => Floor(f.nr, allDevices)
      case f => Floor(f.nr)
    }, 0, List.empty).hash.drop(2)
  }

  println(s"Starting solver...")
  val startTime = System.currentTimeMillis()

//  println("\n-------------------------\n")
//  println(top(1, startState.nextStates).mkString("\n------------------------\n"))

//  println(multiStateToString(startState.nextStates.flatMap(_.nextStates), 4))

  val res = Utils.solve[State, String](startState, _.hash.drop(2) == doneHash, _.nextStates, _.hash)

//  val res = solve(List(startState), Set(startState.hash))
  println(res)
  println(res.hash)
  println(s"Moves: ${multiStateToString((res :: res.prevStates).reverse)}")
//  println(doneHash)
  println(s"Time consumed: ${System.currentTimeMillis() - startTime}ms")

  def top(n: Int, states: List[State]): List[State] = {
    states.sortBy(s => -s.floors.map(f => f.devices.length * f.nr).sum).take(n)
  }

  def multiStateToString(states: List[State], columns: Int = 4) = {
    def multiState(s: List[State]) =
      s.flatMap(_.toString.split("\n").toList.map(_.padTo(75, " ")).transpose).transpose.map(_.mkString).mkString("\n")
    states.grouped(columns).map(multiState).mkString("\n\n")
  }

  @tailrec
  def solve(tries: Iterable[State], explored: Set[String] = Set.empty): State = {
    println(s"states: ${tries.size}, explored: ${explored.size}")
    tries.find(s => s.hash.drop(2) == doneHash) match {
      case None =>
        val newTries = tries.flatMap(_.nextStates).filterNot(s => explored(s.hash))
        val distinctNewTries = newTries.groupBy(_.hash).map(_._2.head)
        println(s"Less tries after distinct: ${newTries.size - distinctNewTries.size}")
        solve(distinctNewTries, explored ++ newTries.map(_.hash))
      case Some(x) =>
        x
    }
  }

  def fromMatch(m: Match, floor: Floor): Floor = {
    m.group(3) match {
      case x if x.contains("microchip") => floor.copy(devices = MicroChip(m.group(2)) :: floor.devices)
      case x if x.contains("generator") => floor.copy(devices = Generator(m.group(2)) :: floor.devices)
    }
  }

  def fromString(str: String): Int = str match {
    case s if s.matches("first") => 0
    case s if s.matches("second") => 1
    case s if s.matches("third") => 2
    case s if s.matches("fourth") => 3
  }

  def done(floors: List[Floor]): Boolean = {
    floors.filter(_.nr < 3).forall(_.devices.isEmpty)
  }

}
