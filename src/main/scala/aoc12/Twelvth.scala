package aoc12

import util.Utils

object Twelvth extends App {
  val input = Utils.load("/input12.txt", getClass).filterNot(_.isEmpty)
//  println(input.mkString("\n"))
  val cpyPattern = "cpy (\\S+) (\\S+)".r
  val jnzPattern = "jnz (\\S+) (\\S+)".r
  val incPattern = "inc (\\S+)".r
  val decPattern = "dec (\\S+)".r

  def process(instructions: List[String], prevInstructions: List[String] = List.empty, registry: Map[Char, Int] = Map()): Map[Char, Int] = {
//    println(s"${instructions.length}:${prevInstructions.length}")
    instructions match {
      case Nil => registry
      case (cur@cpyPattern(from, to)) :: tail =>
        process(tail, prevInstructions :+ cur, copy(from, to, registry))
      case (cur@jnzPattern(from, to)) :: tail =>
        if (from.head.isLetter && registry.getOrElse(from.head, 0) != 0 || (from.matches("\\d+") && from.head.asDigit > 0)) {
          val (prev, next) = movePointer(prevInstructions, cur, tail, to.toInt)
          process(next, prev, registry)
        } else process(tail, prevInstructions :+ cur, registry)
      case (cur@incPattern(variable)) :: tail =>
        val c = variable.head
        process(tail, prevInstructions :+ cur, registry.updated(c, registry.getOrElse(c, 0) + 1))
      case (cur@decPattern(variable)) :: tail =>
        val c = variable.head
        val newReg = registry.updated(c, registry.getOrElse(c, 0) - 1)
//        println(s"newReg: $newReg")
        process(tail, prevInstructions :+ cur, newReg)
    }
  }

  def movePointer(prev: List[String], cur: String, next: List[String], move: Int): (List[String], List[String]) = {
    val index = prev.length
    val total = prev ++ (cur :: next)

//    println(index)
//    println(total.length)
//    println(s"moving pointer from $index to ${index + move}")
    val newIndex = index + move
    val p = total.take(newIndex)
    val n = total.drop(newIndex)
//    println( (p ++ n).length)
    p -> n
  }


  val solution1 = process(input)
  println(solution1('a'))

  val solution2 = process(input, List.empty, Map('c'->1))
  println(solution2('a'))

  def copy(from: String, to: String, reg: Map[Char, Int]): Map[Char, Int] = {
    (from, to) match {
      case (a, b) if a.matches("-*\\d+") && b.matches("[a-z]") =>
        reg.updated(b.head, a.toInt)
      case (a, b) if a.matches("[a-z]") && b.matches("[a-z]") =>
        reg.updated(b.head, reg.getOrElse(a.head, 0))
    }
  }




}
