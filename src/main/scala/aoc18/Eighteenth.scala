package aoc18


object Eighteenth extends App {
  val input = "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^.^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^"
  val example = "..^^."
  val funny =".................................................^.................................................."

//  val traps = input.toList
//  println(traps.slice(1 - 1, 1 + 2))
//  println(example.toList)
  val start = System.currentTimeMillis()
  println(getRows(40, input.toList, 0))
  println(getRows(400000, input.toList, 0))
  println(s"time consumed: ${System.currentTimeMillis() - start}ms")
//  println(allRows(100, funny.toList).map(_.map{case '.' => ' ' case x => x}.mkString).mkString("\n"))
  def getRows(rows: Int, last: List[Char], total: Int): Int = {
//    println(total)
    if (rows == 0) total
    else {
      val next = last.zipWithIndex.map {
        case (trap, i) if i == 0 =>
          isTrap('.' :: last.take(2))
        case (trap, i) if i == last.length - 1 =>
          isTrap(last.drop(last.length - 2) :+ '.')
        case (trap, i) =>
          isTrap(last.slice(i - 1, i + 2))
      }

      getRows(rows - 1, next, total + last.count(_ == '.'))
    }
  }

  def allRows(rows: Int, last: List[Char]): List[List[Char]] =
    if (rows == 0) List(last)
    else {
      val next = last.zipWithIndex.map {
        case (trap, i) if i == 0 =>
          isTrap('.' :: last.take(2))
        case (trap, i) if i == last.length - 1 =>
          isTrap(last.drop(last.length - 2) :+ '.')
        case (trap, i) =>
          isTrap(last.slice(i - 1, i + 2))
      }
      last :: allRows(rows - 1, next)
    }

  def isTrap(t: List[Char]) = t match {
    case List('^','^','.') => '^'
    case List('.','^','^') => '^'
    case List('^','.','.') => '^'
    case List('.','.','^') => '^'
    case _ => '.'
  }
}
