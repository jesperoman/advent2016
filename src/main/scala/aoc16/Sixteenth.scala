package aoc16

object Sixteenth extends App {
  val input = "11101000110010100"

  def dragonCurve(s: String, length: Int): String = {
    val res = s + "0" + s.map {
      case '0' => '1'
      case '1' => '0'
    }.reverse
    if (res.length >= length) res.take(length)
    else dragonCurve(res, length)
  }

  def checkSum(s: String): String = {
    s.grouped(2).map (_.toList).map{
      case Seq(c1, c2) if c1 == c2 => '1'
      case _ => '0'
    }.mkString
  }

  def oddCheckSum(c: String): String = {
    println(c.length)
    if (c.length % 2 != 0) {
      c
    }
    else {
      oddCheckSum(checkSum(c))
    }
  }

  val start = System.currentTimeMillis()
  println(oddCheckSum(dragonCurve(input, 272)))
  println(oddCheckSum(dragonCurve(input, 35651584)))
  println(s"Time: ${System.currentTimeMillis() - start}ms")
//  println(oddCheckSum())
//  println(oddCheckSum(dragonCurve(input, 35651584)))
//  println(checkSum(dragonCurve("1")))
//  println(dragonCurve("1"))
}
