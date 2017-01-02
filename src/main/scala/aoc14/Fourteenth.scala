package aoc14

import util.Utils

object Fourteenth extends App {
  com.twmacinta.util.MD5.initNativeLibrary(true)

  val tsPattern = "((.)\\2{2})".r
  val pattern = "[a-z]+(\\d+)".r

  def solution1(input: String) = {
    val indexes = Stream.from(0)
    val iStream = indexes.map(input + _)
    val hashes = iStream.map(x => x -> Utils.fastMD5(x))
    hashes.map {
      case (i, h) => (i, h, tsPattern.findAllMatchIn(h).map(_.matched.head).toList)
    }.filter {
      case (i, h, ms) => ms.nonEmpty
    }.collect {
      case (index@pattern(idx), h, ms) if hashes.slice(idx.toInt + 1, idx.toInt + 1000).exists(_._2.matches(s".*${ms.head}{5}.*")) =>
        (index, h, ms, hashes.slice(idx.toInt + 1, idx.toInt + 1000).filter(_._2.matches(s".*(${ms.map(_ + "{5}").mkString("|")}).*")).map(_._2).toList)
    }
  }

  def solution2(input: String, stretch: Int = 2017) = {
    val hashes = Stream.from(0).map(input + _).map(x => x -> Utils.stretchedMD5(x, stretch))
    val result = hashes.map {
      case (i, h) => (i, h, tsPattern.findFirstMatchIn(h).map(_.matched.head))
    }
      .filter(_._3.nonEmpty)
      .filter {
        case (index@pattern(idx), h, Some(m)) =>
          val p = s".*$m{5}.*"
          val i = idx.toInt
          val window = hashes.slice(i + 1, i + 1000)
          window.exists(_._2.matches(p))
      }
        .map{
          case (index@pattern(idx), _, _) =>
            idx.toInt
        }

    result.take(64).toList.reverse.head
  }

  val input_orig = "jlmsuwbz"
  val input_example = "abc"


  val start = System.currentTimeMillis()
  println(solution2(input_orig, 1))
  println(solution2(input_orig, 2017))
  println(s"time consumed: ${System.currentTimeMillis() - start}ms")
}
