package aoc22

import util.Utils

import scala.annotation.tailrec

object Twentysecond extends App {
  type Grid = List[List[Node]]
  val input = Utils.load("/input22.txt", getClass)
  val pattern = "/dev/grid/(node-x(\\d+)-y(\\d+))\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)%".r
  val disks = input.collect {
    case pattern(name, x, y, size, used, avail, percent) =>
      Node(name, Pos(x.toInt, y.toInt), used.toInt, avail.toInt)
  }

  val nodePattern = "node-x(\\d+)-y(\\d+)".r
  val startGrid = disks.groupBy(_.pos.y).toList.sortBy(_._1).map(_._2).map(_.sortBy(_.pos.x))

  val disksWithPossibles = disks.foldLeft(List.empty[(Node, List[Node])]) { (pairs, node) =>
    val disksWithSpace = disks.filter(d => d.avail >= node.used && d != node)
    if (disksWithSpace.nonEmpty && node.used > 0)
      node -> disksWithSpace :: pairs
    else pairs
  }
  println(disksWithPossibles.length)


  val fromDisk = getDisk(startGrid)(Pos(startGrid.head.length - 1, 0))
  val goalDisk = getDisk(startGrid)(Pos(0,0))


  val route = findRoute(startGrid, fromDisk, goalDisk, fromDisk.pos)

  println(moveData(startGrid, route.reverse, fromDisk.pos)._2)

  def moveData(grid: Grid, route: List[Pos], except: Pos, steps: Int = 0): (Grid, Int) = {
    route match {
      case Nil => grid -> steps
      case p :: Nil => grid -> steps
      case f :: t :: tail =>
        val to = getDisk(grid)(t)
        val from = getDisk(grid)(f)
        val (newGrid, ss) =
          if (to.avail >= from.used)
            move(grid, f, t) -> 1
          else {
            val (ng, s) = clear(grid, t, f)
            move(ng, f, t) -> (1 + s)
          }
        moveData(newGrid, t :: tail, t, steps + ss)
    }
  }


  def clear(grid: Grid, pos: Pos, except: Pos): (Grid, Int) = {
    val node = getDisk(grid)(pos)
    val ws = disksWithSpace(grid, node)
    val route = ws.map(n => findRoute(grid, n, node, except)).minBy(_.length).reverse
    moveRoute(grid, route)
  }

  @tailrec
  def moveRoute(grid: Grid, ps: List[Pos], steps: Int = 0): (Grid, Int) = {
//    println(gridToString(grid.slice(ps.head.y, ps.head.y + 1)))
    ps match {
      case Nil => grid -> steps
      case h :: Nil => grid -> steps
      case to :: from :: tail =>
//        println(s"move from ($from) to ($to)")
        val newGrid = move(grid, from, to)
//        println(gridToString(newGrid.slice(from.y, from.y + 1)))
        moveRoute(newGrid, from :: tail, steps + 1)
    }
  }

  def findRoute(grid: Grid, from: Node, to: Node, except: Pos) = {
    def loop(routes: List[List[Node]], visited: Set[Node] = Set.empty): List[Pos] = {
      routes.find(_.head == to) match {
        case Some(n) => n.map(_.pos)
        case None =>
          val newPossibles = routes.flatMap { r =>
            neighborsThatWouldFit(grid, r.head).filterNot(x => visited(x) || x.pos == except).map(_ :: r)
          }.groupBy(_.head.pos).map(_._2.head).toList
          loop(newPossibles, visited ++ newPossibles.map(_.head))
      }
    }
    loop(List(List(from)), Set(from))
  }

  def disksWithSpace(grid: Grid, node: Node) = {
    grid.flatten.filter { n =>
      n != node && n.avail >= node.used
    }
  }

  def getDisk(grid: Grid)(p: Pos) = grid(p.y)(p.x)

  def gridToString(grid: Grid) = {
    grid.map(_.mkString(" -- ")).mkString("\n")
  }

  def move(grid: Grid, fp: Pos, tp: Pos): Grid = {
    val from = getDisk(grid)(fp)
    val to = getDisk(grid)(tp)
    if (getNeighbors(grid, fp).contains(to) && from.used <= to.avail) {
      val newFrom = from.copy(used = 0, avail = from.avail + from.used)
      val newTo = to.copy(used = to.used + from.used, avail = to.avail - from.used)
      val newGrid = updateNode(grid, from.pos, newFrom)
      updateNode(newGrid, to.pos, newTo)
    } else {
      throw new Exception(s"Not possible to move $from to $to")
    }
  }

  def updateNode(grid: Grid, pos: Pos, newNode: Node): Grid = {
    grid.updated(pos.y, grid(pos.y).updated(pos.x, newNode))
  }

  def neighborsWithSpace(grid: Grid, n: Node): Seq[Node] =
    getNeighbors(grid, n.pos).filter(_.avail >= n.used)

  def neighborsThatWouldFit(grid: Grid, node: Node): Seq[Node] =
    getNeighbors(grid, node.pos).filter(n => n.used <= node.used + node.avail)

  def getNeighbors(grid: Grid, p: Pos): Seq[Node] = {
    val maxX = grid.head.length - 1
    val maxY = grid.length - 1
    val Pos(x, y) = p
    List(
      x - 1 -> y,
      x + 1 -> y,
      x -> (y + 1),
      x -> (y - 1)
    ).collect {
      case (x, y) if y >= 0 && y <= maxY && x >= 0 && x <= maxX =>
        getDisk(grid)(Pos(x, y))
    }
  }
}
