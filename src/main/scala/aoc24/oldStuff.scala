package aoc24

/**
  * Created by jesper on 2016-12-29.
  */
object oldStuff {
  /*
  def findRoute(tries: Iterable[MState], visited: Set[(Int, Int)] = Set.empty): Int = {
//    println(tries.size)
    val x = tries.flatMap {
      case s =>
        getPossibleNextMoves(s).filterNot(s => visited(s.pos))
    }.groupBy(_.pos).map(_._2.toList.sortBy(_.moves).head)
    x.find (s => getChar(s.pos).toString.matches("\\d") && !s.visitedNrs.contains(getChar(s.pos).asDigit)) match {
      case Some(s@MState(pos, moves, vNr)) if vNr.length + 1 >= goal.length =>
//        println(s"done!")
        println(s"Done: $moves moves! $pos,nrs:${getChar(pos) :: vNr}")
        moves
      case Some(s@MState(pos, moves, vNr)) if !vNr.contains(getChar(pos).asDigit) =>
//        println(s"found digit: ${getChar(pos)}")
//        println(s"$moves moves! $pos, nrs:$vNr")
        findRoute(Iterable(s.copy(visitedNrs = getChar(pos).asDigit :: vNr)), Set(pos))
      case None => findRoute(x, visited ++ x.map(_.pos))
    }
  }
*/

}
