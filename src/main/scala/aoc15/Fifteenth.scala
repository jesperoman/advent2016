package aoc15

import util.Utils

object Fifteenth extends App {
  val input = Utils.load("/input15.txt", getClass)
  val p = "Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+).".r
  val discs = input.map{
    case p(nr, ps, start) => Disc(nr.toInt, ps.toInt, start.toInt)
  }

  val start = System.currentTimeMillis()
  val res1 = Stream.from(0).map(time => time -> discs.map(_.atTime(time))).filter(_._2.forall(_.current == 0)).take(1).head._1
  val discs2 = Disc(7, 11, 0) :: discs
  val res2 = Stream.from(0).map(time => time -> discs2.map(_.atTime(time))).filter(_._2.forall(_.current == 0)).take(1).head._1
  println(res1)
  println(res2)
  println(s"time consumed: ${System.currentTimeMillis() - start}")

}
