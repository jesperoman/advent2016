package aoc20

import util.Utils

object Twentieth extends App {
  val input = Utils.load("/input20.txt", getClass)

  val example =
    """5-8
      |0-2
      |4-7""".stripMargin.split("\n").toList
  val p = "(\\d+)\\-(\\d+)".r
  val ranges = input.map {
    case p(f, t) => f.toLong -> t.toLong
  }

//  val rs = ranges.foldLeft(List.empty[(Long, Long)]) { (se, excl) =>
//    excl match {
//      case o@(f, t) =>
//        (getRange(se, f), getRange(se, t)) match {
//          case (Some(fst@(f1, t1)), Some(scnd@(f2, t2))) => f1 -> t2 :: se.filterNot(r => r == fst || r == scnd)
//          case (Some(fst@(f1, t1)), None) => f1 -> t :: se.filterNot(_ == fst)
//          case (None, Some(scnd@(f2, t2))) => f -> t2 :: se.filterNot(_ == scnd)
//          case (None, None) => o :: se
//        }
//      }
//    }.sortBy(_._1)

  val rs = sanitize(List.empty, ranges.sortBy(_._1)).sortBy(_._1)

  def sanitize(bef: List[(Long, Long)], after: List[(Long, Long)]): List[(Long, Long)] = {
    after match {
      case Nil => bef
      case (o@(f, t)) :: tail =>
        val rs = bef ++ tail
        val newBef = (getRange(rs, f), getRange(rs, t)) match {
          case (Some(fst@(f1, t1)), Some(scnd@(f2, t2))) => f1 -> t2 :: bef.filterNot(r => r == fst || r == scnd)
          case (Some(fst@(f1, t1)), None) => f1 -> t :: bef.filterNot(_ == fst)
          case (None, Some(scnd@(f2, t2))) => f -> t2 :: bef.filterNot(_ == scnd)
          case (None, None) => o :: bef
        }
        sanitize(newBef, tail)
    }
  }



  def getRange(ranges: List[(Long, Long)], x: Long): Option[(Long, Long)] = {
    ranges.find {
      case (f, t) => x >= f && x <= t
    }
  }


  val isExcluded: (Int) => Boolean = {
    rs.foldLeft[Int => Boolean](_ < 0) { (p, r) =>
      r match {
        case (f, t) => x => p(x) || (x >= f && x <= t)
      }
    }
  }

//  println(rs.take(10))
//  println(s"excludedF done: ${rs.length}")

  val firstIp = Stream.from(0).filterNot(isExcluded).head
  println(firstIp)
  def findHoles(rs: List[(Long, Long)], last: Long, holes: Long, max: Long): Long = rs match {
    case Nil => holes + max - last
    case (f, t) :: tail =>
//      println(s"$f, $t, $last, $holes, $max")
      findHoles(tail, t, (holes + f - last - 1).max(0), max)
  }

  val allowed = findHoles(rs, 0, 0, 4294967295L)

  println(allowed)

}
