package aoc23

import util.Utils

object Twentythird  extends App {
  val input = Utils.load("/input23.txt", getClass)
//  val input12 = Utils.load("input12.txt", getClass)
  val cpyPattern = "cpy (\\S+) (\\S+)".r
  val jnzPattern = "jnz (\\S+) (\\S+)".r
  val incPattern = "inc (\\S+)".r
  val decPattern = "dec (\\S+)".r
  val tglPattern = "tgl (\\S+)".r
//  println(input.take(3))

  val start = System.currentTimeMillis()
  println(s"Part 1: ${interpret(0, input, 0, Map('a' -> 7))('a')}")
  println(s"Part 2: ${interpret(0, input, 0, Map('a' -> 12))('a')}")
  println(s"Time consumed: ${System.currentTimeMillis() - start}ms")
  def interpret(iter:Int, is: List[String], pos: Int = 0, reg: Map[Char, Int] = Map.empty.withDefaultValue(0)): Map[Char, Int] = {
//    println(s"iterations: $iter")
    if (pos >= is.length) reg
    else {
      is(pos) match {
        case cpyPattern(from, to) =>
          interpret(iter + 1, is, pos + 1, copy(from, to, reg))
        case i@jnzPattern(test, movePos) =>
          val t =
            if (test.head.isLetter) reg(test.head) else test.head.asDigit
          if (t != 0 && getInt(movePos, reg) != 0)
            interpret(iter + 1, is, pos + getInt(movePos, reg), reg)
          else interpret(iter + 1, is, pos + 1, reg)
        case incPattern(v) =>
          interpret(iter + 1, is, pos + 1, reg.updated(v.head, reg(v.head) + 1))
        case decPattern(v) =>
          interpret(iter + 1, is, pos + 1, reg.updated(v.head, reg(v.head) - 1))
        case i@tglPattern(p) =>
          val ptt = pos + (if (p.head.isLetter) {
            reg(p.head)
          } else {
            p.toInt
          })
          val newIs = if (ptt >= 0 && ptt < is.length)
            is.updated(ptt, toggle(is(ptt)))
          else is
          interpret(iter + 1, newIs, pos + 1, reg)
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

  val reg = Map (
    'a' -> 1,
    'b' -> 2,
    'c' -> 3,
    'd' -> 4
  )

//  println(input.map(i => s"${i.padTo(10, " ").mkString}${toggle(i)}").mkString("\n"))

}
