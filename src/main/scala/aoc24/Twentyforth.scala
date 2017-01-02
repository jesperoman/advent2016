package aoc24

import util.Utils

object Twentyforth extends App {
  val input = Utils.load("/input24.txt", getClass)
//  println(input.mkString("\n"))
  val goal = input.flatMap(_.collect{case x if x.isDigit => x.asDigit})
  println(goal)
  val startNr = 0
  val startPos = input.zipWithIndex.find(_._1.matches(s".*$startNr.*")).map(
    s => s._1.indexOf(s"$startNr") -> s._2
  ).get

  val startState = MState(startPos, 0, List(startNr))

  println(s"Answer 2: ${findRoutesSolve(startState, goal).moves}")

  println(s"Time consumed: ${System.currentTimeMillis() - this.executionStart}ms")

  def findRoutesSolve(start: MState, g: List[Int]): MState = {
    Utils.solve[MState, List[Int]](
      start,
      _.visitedNrs.length == g.length,
      s => g.diff(s.visitedNrs).map(nr => findRouteToNrSolve(nr, s)),
      s => s.visitedNrs,
      debug = true,
      { l =>
        println(s"Answer 1: ${l.minBy(_.moves).moves}")
        l.map(s => findRouteToNrSolve(0, s)).minBy(_.moves)
      }
    )
  }

  def findRouteToNrSolve(nr: Int, start: MState): MState = {
    val s = Utils.solve[MState, (Int, Int)](
      start,
      s => getChar(s.pos).toString.matches("\\d") && getChar(s.pos).asDigit == nr,
      getPossibleNextMoves,
      _.pos
    )
    s.copy(visitedNrs = getChar(s.pos).asDigit :: s.visitedNrs)
  }

  def findRoutes(routes: List[MState]): MState = {
    println(routes.length + s" consumed: ${System.currentTimeMillis() - this.executionStart}ms")
    if (routes.exists(_.visitedNrs.length == goal.length)) {
      println(s"Shortest route with all numbers: ${routes.minBy(_.moves)}")
      println(s"Found ${routes.length} routes with all nrs, finding best way back")
      val x = routes.map { s =>
        findRouteToNrSolve(0, s)
      }
      x.minBy(_.moves)
    } else {
      //      println(routes)
      val newStates = routes.flatMap(x =>
        goal.diff(x.visitedNrs).map { nr =>
          findRouteToNrSolve(nr, x)
          //          findRouteToNr(nr, List(x), Set(x.pos))
        }
      )
      //      println(newStates)
      findRoutes(newStates)
    }
  }

  def findRouteToNr(nr: Int, tries: Iterable[MState], visited: Set[(Int, Int)]): MState = {
    val x = tries.flatMap {
      case s =>
        getPossibleNextMoves(s).filterNot(s => visited(s.pos))
    }.groupBy(_.pos).map(_._2.toList.sortBy(_.moves).head)
    x.find (s => getChar(s.pos).toString.matches("\\d") && getChar(s.pos).asDigit == nr) match {
      case Some(s@MState(pos, moves, vNr)) =>
//        println(s"Done: $moves moves! $pos,nrs:${getChar(pos) :: vNr}")
        s.copy(visitedNrs = getChar(pos).asDigit :: vNr)
      case None => findRouteToNr(nr, x, visited ++ x.map(_.pos))
    }
  }



  def getChar(pos: (Int, Int)): Char = {
    pos match {
      case (x, y) => input(y)(x)
    }
  }

  def getPossibleNextMoves(s: MState): List[MState] = {
    val nextPoss = s.pos match {
      case (x, y) =>
        List(
          (x + 1) -> y,
          (x - 1) -> y,
          x -> (1 + y),
          x -> (y - 1)
        ).filterNot(isWall)
    }
    nextPoss.map(p => s.copy(pos = p, moves = s.moves + 1))
  }

  def isWall(pos: (Int, Int)) = pos match {
    case (x, y) =>
      input(y)(x) == '#'
  }
}
