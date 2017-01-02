package aoc17

import util.Utils

object Seventeenth extends App {
  val input = "pxxbnzuo"
  val example = "ihgpwlah"
  val example2 = "kglvqrro"
  val example3 = "ulqzkmiv"

  val minPath = findPath(3 -> 3)(List((0,0) -> input)).head.drop(input.length)
  println(minPath)
  val maxPathLength = findPath(3 -> 3)(List((0,0) -> input)).map(_.drop(input.length).length).max
  println(maxPathLength)

  def findPath(goal: (Int, Int))(paths: List[((Int, Int), String)]): List[String] = {
    val solutions = paths.filter(_._1 == goal).map(_._2)

    val notYet = paths.filterNot(_._1 == goal)

    if (notYet.nonEmpty) solutions ++ findPath(goal)(notYet.flatMap {
      case ((x,y), route) => possibleNext(x -> y, route)
    })
    else solutions
  }

  def doorsOpen(s: String): Seq[Char] = {
    Utils.fastMD5(s).take(4).zip("UDLR").collect {
      case (h, dir) if "bcdef" contains h => dir
    }
  }

  def possibleNext(pos: (Int, Int), prev: String) = {
    pos match {
      case (x, y) =>
        doorsOpen(prev) collect {
          case d@'L' if x - 1 >= 0 => (x - 1, y) -> (prev + d.toString)
          case d@'R' if x + 1 <= 3 => (x + 1, y) -> (prev + d.toString)
          case d@'D' if y + 1 <= 3 => (x, y + 1) -> (prev + d.toString)
          case d@'U' if y - 1 >= 0 => (x, y - 1) -> (prev + d.toString)
        }
    }
  }

}
