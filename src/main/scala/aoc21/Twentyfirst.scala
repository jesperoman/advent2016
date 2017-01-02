package aoc21

import util.Utils

object Twentyfirst extends App {
  val input = Utils.load("/input21.txt", getClass)

  val swap = "swap position (\\d) with position (\\d)".r
  val swapLetter = "swap letter (\\w) with letter (\\w)".r
  val reverse = "reverse positions (\\d) through (\\d)".r
  val rotate = "rotate (\\w+) (\\d) steps*".r
  val rotateBased = "rotate based on position of letter (\\w)".r
  val move = "move position (\\d) to position (\\d)".r

  val password = "abcdefgh"

  val exampleInstructions =
    """swap position 4 with position 0
      |swap letter d with letter b
      |reverse positions 0 through 4
      |rotate left 1 step
      |move position 1 to position 4
      |move position 3 to position 0
      |rotate based on position of letter b
      |rotate based on position of letter d""".stripMargin.split("\n").toList.map(_.trim)

  def scramble(p: String, i: List[String]) = {
    i.foldLeft(p) { (pw, instr) =>
      //    println(pw)
//      println(instr)
      val newP = instr match {
        case swap(x, y) =>
          swap(pw, x.toInt, y.toInt)
        case swapLetter(x, y) =>
          //        println(s"swap letter $x with letter $y")
          //        println(s"swap($pw, ${pw.indexOf(x)}, ${pw.indexOf(y)})")
          swap(pw, pw.indexOf(x), pw.indexOf(y))
        case reverse(x, y) =>
          reverse(pw, x, y)
        case rotate("left", x) =>
          rotateLeft(pw, x.toInt)
        case rotate("right", x) =>
          rotateRight(pw, x.toInt)
        case rotateBased(x) =>
          rotateBasedOnLetterIndex(pw, x)
        case move(x, y) =>
          move(pw, x, y)
      }
//      println(pw + " " + instr + " " + newP)
      newP
    }
  }

  def descramble(p: String, i: List[String]) = {
    i.reverse.foldLeft(p) { (pw, instr) =>
//      println(instr)
      val newP = instr match {
        case swap(y, x) =>
          swap(pw, x.toInt, y.toInt)
        case swapLetter(y, x) =>
          swap(pw, pw.indexOf(x), pw.indexOf(y))
        case reverse(x, y) =>
          reverse(pw, x, y)
        case rotate("right", x) =>
          rotateLeft(pw, x.toInt)
        case rotate("left", x) =>
          rotateRight(pw, x.toInt)
        case rotateBased(x) =>
          rotateBasedReverse(pw, x)
        case move(y, x) =>
          move(pw, x, y)
      }
//      println(pw + " " + instr + " " + newP)
      newP
    }
  }

  def swap(pw: String, x: Int, y: Int) =
    pw.updated(x.toInt, pw.charAt(y.toInt))
      .updated(y.toInt, pw.charAt(x.toInt))

  def rotateRight(str: String, nr: Int) = {
    val r = nr % str.length
    str.drop(str.length - r) + str.take(str.length - r)
  }
  def rotateLeft(pw: String, x: Int) = {
    val n = x %  pw.length
    pw.drop(n) + pw.take(n)
  }



  def reverse(pw: String, x: String, y: String) =
    pw.take(x.toInt) + pw.slice(x.toInt, y.toInt+1).reverse + pw.drop(y.toInt + 1)

  def move(pw: String, x: String, y: String) = {
    val newPw = pw.take(x.toInt) + pw.drop(x.toInt + 1)
    newPw.take(y.toInt) + pw.charAt(x.toInt) + newPw.drop(y.toInt)
  }

  def rotateBasedOnLetterIndex(pw: String, x: String) = {
    val idx = pw.indexOf(x)
    val rightRotation = idx + 1 + (if(idx >=4) 1 else 0)
//    println(rightRotation)
    rotateRight(pw, rightRotation)
  }

  val p = "abcdefg"

//println(rotateRight("ecabd", 6))
//  val examplepw = scramble("abcde", exampleInstructions)
//  println(examplepw)
//  println(descramble(examplepw, exampleInstructions))

//  println(s"Scramble $password\n==========================================")
  val scrambled = scramble(password, input)
  println(scrambled)
//  println(s"descramblind $scrambled\n=========================================")
//  val descrambled = descramble(scrambled, input)
//  println(descrambled)
  println(descramble("fbgdceah", input))

  def rotateBased(pw: String, x: String): String = {
    val idx = pw.indexOf(x)
    val rightRotation = idx + 1 + (if(idx >=4 ) 1 else 0)
    //          println(rightRotation)
    rotateRight(pw, rightRotation)
  }

  def rotateBasedReverse(pw: String, x: String): String = {
    val rr = (0 to pw.length).find { i =>
      val rotated = rotateLeft(pw, i)
      pw == rotateBased(rotated, x)
    }.get
//    println(rr)
//    println(rotateBased(rotateLeft(pw, rr), x))
    rotateLeft(pw, rr)
  }

//  println(p)
//  println(rotateBased(p, "b"))
//  println(rotateBasedReverse(p, "b"))
}
