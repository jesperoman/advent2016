package aoc08

import aoc08.Instruction.{Rect, RotateColumn, RotateRow}
import util.Utils

sealed trait Instruction

object Instruction {
  case class RotateRow(y: Int, step: Int) extends Instruction
  case class RotateColumn(x: Int, step: Int) extends Instruction
  case class Rect(x: Int, y: Int) extends Instruction
  def fromString(s: String): Instruction = {
    val rectP = "rect (\\d+)x(\\d+)".r
    val rotR = "rotate row y=(\\d+) by (\\d+)".r
    val rotC = "rotate column x=(\\d+) by (\\d+)".r
    s match {
      case rectP(x, y) => Rect(x.toInt, y.toInt)
      case rotR(y, step) => RotateRow(y.toInt, step.toInt)
      case rotC(x, step) => RotateColumn(x.toInt, step.toInt)
    }
  }
}

object Eighth extends App {
  val input = Utils.load("/input8.txt", getClass)
  val instructions = input.map(Instruction.fromString)

  val pixels = instructions.foldLeft(List.fill(6, 50)(' ')) { (disp, instr) =>
    instr match {
      case r: Rect => addRect(disp, r)
      case r: RotateRow => rotateRow(disp, r)
      case r: RotateColumn => rotateColumn(disp, r)
    }
  }

  println("Part1: " + pixels.map(_.count(_ == '*')).sum)

  println("Part2:\n" + pixels.map(_.mkString).mkString("\n"))

  def rotateRow[A](disp: List[List[A]], r: RotateRow): List[List[A]] =
    disp.updated(r.y, disp(r.y).drop(disp(r.y).length - r.step) ++ disp(r.y).take(disp(r.y).length - r.step))

  def rotateColumn[A](disp: List[List[A]], r: RotateColumn): List[List[A]] = {
    val col = disp.map(_(r.x))
    val newCol = col.drop(col.length - r.step) ++ col.take(col.length - r.step)
    col.indices.foldLeft[List[List[A]]](disp) { (d, idx) =>
      d.updated(idx, d(idx).updated(r.x, newCol(idx)))
    }
  }

  def addRect(disp: List[List[Char]], rect: Rect): List[List[Char]] = {
    val coordinates = for {
      x <- 0 until rect.x
      y <- 0 until rect.y
    } yield x -> y

    coordinates.foldLeft(disp) { (disp, coord) =>
      coord match {
        case (x, y) => disp.updated(y, disp(y).updated(x, '*'))
      }
    }
  }



//  println (List.fill(6, 50)(' ').mkString("\n"))


//  println(input.take(5).mkString("\n"))
}
