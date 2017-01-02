package aoc09

import util.Utils

object Ninth extends App {
  val input = Utils.load("/input9.txt", getClass).head

  def decompLength(s: String, length: Long): Long = {
    val marker = """\((\d+)x(\d+)\)""".r
    val matches = marker.findFirstMatchIn(s)
    matches.map { m =>
      val beforeLen = m.before.length

      val chars = m.group(1).toInt
      val repeat = m.group(2).toInt
      val matched = m.matched.length

      val len = decompLength(m.after.toString.take(chars), 0)
      val curLen = len * repeat + beforeLen
      decompLength(s.drop(matched + beforeLen + chars), curLen + length)
    }.getOrElse(s.length + length)
  }

  def decompress(s: String): String = {
    val marker = """\((\d+)x(\d+)\)""".r
    val matches = marker.findFirstMatchIn(s)
    matches.map { m =>
      val chars = m.group(1).toInt
      val repeat = m.group(2).toInt
      val toRepeat = m.after.toString.take(chars)
      val before = m.before.toString
      before + (1 to repeat).toList.map(_=>toRepeat).foldLeft("")(_ + _) + decompress(m.after.toString.drop(chars))
    }.getOrElse(s)
  }
  val decompressed = decompress(input).replaceAll("\\s", "")
  println(s"Part 1: ${decompressed.length}")
  println(s"Part 2: ${decompLength(input, 0)}")
}
