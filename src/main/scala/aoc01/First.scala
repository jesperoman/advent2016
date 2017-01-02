package aoc01

import util.Direction
import util.Direction.{Down, Up}

case class Position(dir: Direction, x: Int, y: Int) {
  def left(steps: Int) = newRoute(dir.left, steps)

  def right(steps: Int) = newRoute(dir.right, steps)

  private def newRoute(newDir: Direction, steps: Int): Position =
    newDir match {
      case Up => this.copy(newDir, x, y + steps)
      case Down => this.copy(newDir, x, y - steps)
      case Direction.Right => this.copy(newDir, x + steps, y)
      case Direction.Left => this.copy(newDir, x - steps, y)
    }
  def distance = Math.abs(x) + Math.abs(y)
}


object First extends App {
  val initRoute="""L2,L5,L5,R5,L2,L4,R1,R1,L4,R2,R1,L1,L4,R1,L4,L4,R5,R3,R1,L1,R1,L5,L1,R5,L4,R2,L5,L3,L3,R3,L3,R4,R4,L2,L5,R1,R2,L2,L1,R3,R4,L193,R3,L5,R45,L1,R4,R79,L5,L5,R5,R1,L4,R3,R3,L4,R185,L5,L3,L1,R5,L2,R1,R3,R2,L3,L4,L2,R2,L3,L2,L2,L3,L5,R3,R4,L5,R1,R2,L2,R4,R3,L4,L3,L1,R3,R2,R1,R1,L3,R4,L5,R2,R1,R3,L3,L2,L2,R2,R1,R2,R3,L3,L3,R4,L4,R4,R4,R4,L3,L1,L2,R5,R2,R2,R2,L4,L3,L4,R4,L5,L4,R2,L4,L4,R4,R1,R5,L2,L4,L5,L3,L2,L4,L4,R3,L3,L4,R1,L2,R3,L2,R1,R2,R5,L4,L2,L1,L3,R2,R3,L2,L1,L5,L2,L1,R4"""
  val steps = initRoute.split(",").toList
  val pattern = "([RL])([0-9]+)".r
  val route = steps.foldLeft(List(Position(Up, 0, 0))) { (prev, instr) =>
    instr match {
      case pattern("R", length) =>
        (1 to length.toInt).toList.map(steps => prev.head.right(steps)).reverse ++ prev
      case pattern("L", length) =>
        (1 to length.toInt).toList.map(steps => prev.head.left(steps)).reverse ++ prev
    }
  }.reverse

  println(s"Part 1: ${route.last.distance}")

  val duplicate = findFirstDuplicate(route)

  println(s"Part 2: ${duplicate.distance}")

//  printRoute(route)
  def findFirstDuplicate(route: List[Position], previous: List[Position] = List.empty): Position = {
    route match {
      case Nil => throw new RuntimeException("No solution!")
      case head :: tail if previous.exists(p => p.x == head.x && p.y == head.y) => head
      case head :: tail => findFirstDuplicate(tail, head :: previous)
    }
  }


  def printRoute(route: List[Position]) = {
    val maxY = route.maxBy(_.y).y
    val minY = route.minBy(_.y).y
    val maxX = route.maxBy(_.x).x
    val minX = route.minBy(_.x).x

    (for {
      y <- minY to maxY
      x <- minX to maxX
    } yield {
      x -> y
    }).foreach{
      case (x, y) if route.exists(p => p.x == x && p.y == y) =>
        if (x == maxX) println("*")
        else print("*")
      case (x, _) =>
        if (x == maxX) println(" ")
        else print(" ")
    }
  }
}
