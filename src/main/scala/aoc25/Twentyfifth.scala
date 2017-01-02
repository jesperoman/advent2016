package aoc25

import util.Utils

object Twentyfifth extends App {
  val input = Utils.load("/input25.txt", getClass)
//  println(input)

  val cpyPattern = "cpy (\\S+) (\\S+)".r
  val jnzPattern = "jnz (\\S+) (\\S+)".r
  val incPattern = "inc (\\S+)".r
  val decPattern = "dec (\\S+)".r
  val tglPattern = "tgl (\\S+)".r
  val outPattern = "out (\\S+)".r
  //  println(input.take(3))

  val ts = Stream.from(0).map (i => i -> interpret(List(), input, 0, Map('a' -> i)))

  println(ts.filter(_._2.zipWithIndex.forall {
    case (n, i) => i % 2 == n
  }).head._1)

  def interpret(transmitted: List[Int], is: List[String], pos: Int = 0, reg: Map[Char, Int] = Map.empty.withDefaultValue(0)): List[Int] = {
    if (pos >= is.length) transmitted.reverse
    else if (transmitted.length == 10) {
      transmitted.reverse
    } else {
      is(pos) match {
        case cpyPattern(from, to) =>
          interpret(transmitted, is, pos + 1, copy(from, to, reg))
        case i@jnzPattern(test, movePos) =>
          val t =
            if (test.head.isLetter) reg(test.head) else test.head.asDigit
          if (t != 0 && getInt(movePos, reg) != 0)
            interpret(transmitted, is, pos + getInt(movePos, reg), reg)
          else interpret(transmitted, is, pos + 1, reg)
        case incPattern(v) =>
          interpret(transmitted, is, pos + 1, reg.updated(v.head, reg(v.head) + 1))
        case decPattern(v) =>
          interpret(transmitted, is, pos + 1, reg.updated(v.head, reg(v.head) - 1))
        case tglPattern(p) =>
          val ptt = pos + (if (p.head.isLetter) {
            reg(p.head)
          } else {
            p.toInt
          })
          val newIs = if (ptt >= 0 && ptt < is.length)
            is.updated(ptt, toggle(is(ptt)))
          else is
          interpret(transmitted, newIs, pos + 1, reg)
        case outPattern(to) =>
//          println(s"Transmit: ${getInt(to, reg)}")
          interpret(getInt(to, reg) :: transmitted, is, pos + 1, reg)
      }
    }
  }

  def getInt(s: String, reg: Map[Char, Int]): Int = {
    if(s.head.isLetter) reg(s.head)
    else
    if (s.matches("-*\\d+")) s.toInt
    else 1
  }

  def copy(from: String, to: String, reg: Map[Char, Int]): Map[Char, Int] = {
    (from, to) match {
      case (a, b) if a.matches("-*\\d+") && b.matches("[a-z]") =>
        reg.updated(b.head, a.toInt)
      case (a, b) if a.matches("[a-z]") && b.matches("[a-z]") =>
        reg.updated(b.head, reg.getOrElse(a.head, 0))
      case _ => reg
    }
  }

  def toggle(instr: String): String = instr match {
    case incPattern(x) => s"dec $x"
    case decPattern(x) => s"inc $x"
    case tglPattern(x) => s"inc $x"
    case cpyPattern(x, y) => s"jnz $x $y"
    case jnzPattern(x, y) => s"cpy $x $y"
  }

}
