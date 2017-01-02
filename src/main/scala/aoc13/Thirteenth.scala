package aoc13

import aoc11.State
import util.Utils

object Thirteenth extends App {
  val input = 1358

  val routes = Stream(Stream((1, 1)))

  case class Pos(x: Int, y: Int)




  def findRoutes(goal: Pos, routes: List[List[Pos]], explored: Set[Pos]): List[Pos] = {
    routes.find(_.head == goal) match {
      case Some(x) => x
      case None =>
        val newRoutes = routes.flatMap { p =>
          possibleNextL(p, explored)
        }.groupBy(_.head).toList.map(_._2.head)
        findRoutes(goal, newRoutes, explored ++ newRoutes.map(_.head))
    }
  }

  def findUniquePositions(moves: Int, positions: List[List[Pos]], explored: Set[Pos]): Int = {
    if (moves <= 0) explored.size
    else {
      val newRoutes = positions.flatMap(l => possibleNextL(l, explored)).groupBy(_.head).toList.map(_._2.head)
      findUniquePositions(moves - 1, newRoutes, explored ++ newRoutes.map(_.head))
    }
  }

  val goal = Pos(31,39)
  val start = Stream((Pos(1,1), Nil))

//  println(findRoutes(goal, List(List(Pos(1,1))), Set(Pos(1,1))).tail.length)

  println(Utils.solve[List[Pos], Pos](
    List(Pos(1,1)),
    _.head == goal,
    (l: List[Pos]) => around(l.head).filterNot(isWall).toList.map(_ :: l),
    l => l.head
  ).tail.length)

  println(findUniquePositions(50, List(List(Pos(1,1))), Set(Pos(1,1))))

//  val solution = generateRoutes(goal)(start)
////  val solution1 = solution.find(_._1 == goal).get._2.length
////  println(solution1)
//  val solution2 = solution.map(_._1).toList.length
//  println(solution2)



//  val route = (goal :: generateRoutes(goal)(start).filter(_._1 == goal).head._2).toSet

  def uniquePos(s: Stream[(Pos, List[Pos])]): Stream[(Pos, List[Pos])] =
    s.groupBy(_._1).map { case (p, s) => s.head }.toStream

//  for {
//    y <- -1 to 40
//    x <- -1 to 40
//  } if (x == 40)
//      if (isWall(aoc22.Pos(x, y))) println("*") else println(" ")
//  else
//    if (route(aoc22.Pos(x,y))) print("O")
//  else
//    if (isWall(aoc22.Pos(x, y))) print("*") else print(" ")
//


//  val x = generateRoutes((31,39))(Stream(((1,1), Nil)))

//  println(around(aoc22.Pos(1,1)))
//  def infStream(from: (Int, List[Int])): Stream[(Int, List[Int])] = {
//    from #:: infStream((from._1 + 1, from._1 :: from._2))
//  }



  def possibleNext(point: Pos, previous: List[Pos], explored: Set[Pos]): Stream[(Pos, List[Pos])] = {
    around(point).filterNot(explored).filterNot(isWall).map(_ -> (point :: previous))
  }

  def possibleNextL(route: List[Pos], explored: Set[Pos]): List[List[Pos]] = {
    around(route.head).toList.filterNot(explored).filterNot(isWall).map(_ :: route)
  }

  def around(point: Pos): Stream[Pos] = Stream(
    point.copy(x = point.x - 1),
    point.copy(x = point.x + 1),
    point.copy(y = point.y - 1),
    point.copy(y = point.y + 1)
  )

  def isWall(point: Pos): Boolean =
    point match {
      case Pos(x, y) =>
        x < 0 || y < 0 || (input + (x*x + 3*x + 2*x*y + y + y*y)).toBinaryString.count(_ == '1') % 2 != 0
    }

  /**
    * This seemed to work, copied some old code I had laying around, do not
    * really understand though, remade this... :)
    *
    * @param goal
    * @param rs
    * @param explored
    * @return
    */
  def generateRoutes(goal: Pos)(
    rs: Stream[(Pos, List[Pos])],
    explored: Set[Pos] = Set(Pos(1,1))
  ): Stream[(Pos, List[Pos])] = {
    if (rs.isEmpty)
      Stream.empty
    else {
      val next = for {
        (pos, prev) <- rs
        x <- {
          possibleNext(pos, prev, explored)
        }
        if pos != goal && prev.length < 50
      } yield x

      val newExplored = explored ++ next.map(_._1).toSet

      rs ++ generateRoutes(goal)(uniquePos(next), newExplored)
    }
  }

}
