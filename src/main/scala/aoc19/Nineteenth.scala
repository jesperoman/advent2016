package aoc19

object Nineteenth extends App {
  type Elves = List[(Int, Int)]

  val orig = 3014603
  val example = 5
//  val elves = (1 to orig).map(_ -> 1).toList
//  println (elves)

//  println(s"And the answer id: ${steal(elves)}")

//  println(s"And the answer is: ${stealAcross(elves)}")

  val input = orig

  val elves = (1 to input).toList

  val start = System.currentTimeMillis()
  println(s"Part 1: ${steal((1 to orig).map(_ -> 1).toList)._1}")
  println(s"Part 2: ${stealAcross(elves)}")

  def solve(target: Int) = {
    var i = 1

    while (i * 3 < target)
      i *= 3

    target - i
  }

//  println(s"Crazy result: ${solve(orig)}")
  println(s"Time consumed: ${System.currentTimeMillis() - start}")

  def steal(es: Elves, visited: Elves = List.empty): (Int, Int) = {
    es match {
      case (nr1, p1) :: (nr2, p2) :: tail =>
        steal(tail, nr1 -> (p1 + p2) :: visited)
      case (nr1, p1) :: Nil if visited.nonEmpty =>
        val newEs = visited.reverse
        val newElf = nr1 -> (p1 + newEs.head._2)
        steal(newEs.tail, List(newElf))
      case Nil if visited.length == 1 => visited.head
      case Nil if visited.length > 1 =>
        steal(visited.reverse)
    }
  }

  def stealAcross(es: List[Int]): Int = {

    def loop(fs: List[Int], ss: List[Int]): Int = {
      val fsLen = fs.length
      val ssLen = ss.length
      if (fs.isEmpty || ss.isEmpty)
        Seq(fs, ss).maxBy(_.length).head
      else {
        val res = getKills(fsLen, ssLen)
        val ssi = ss.zipWithIndex
        val newFs = fs.drop(res.size) ++ ssi.collect {
          case (elf, idx) if !res(idx) => elf
        } ++ fs.take(res.size)
        val newLen = newFs.length / 2

        loop(newFs.take(newLen), newFs.drop(newLen))
      }
    }
    val esLen = es.length / 2
    loop(es.take(esLen), es.drop(esLen))
  }


  def getKills(fsLen: Int, ssLen: Int): Set[Int] = {
    (0 until ssLen).foldLeft(Set.empty[Int]) { (kills, cur) =>
      val nrLeft = fsLen - 1 - kills.size
      if (nrLeft <= 0 && kills.nonEmpty) {
        kills
      } else {
        if ((fsLen + ssLen - kills.size) / 2 == cur - kills.size + nrLeft + 1) kills + cur
        else kills
      }
    }
  }



//  def stealAcross(first: Elves, second: Elves, nextFirst: Elves = List.empty, nextSecond: Elves = List.empty): (Int, Int) = {
//    (first, second) match {
//      case (Nil, Nil) => stealAcross(nextFirst, nextSecond)
//      case (f :: _, s :: _) =>
//
//    }
//  }
}
