package aoc05

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.twmacinta.util.MD5
import util.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Fifth extends App {
  def solution1(i: Stream[String]): String =
    i.map(Utils.fastMD5).filter(_.startsWith("00000")).take(8).toList.map(_(5)).mkString

  def solution2(i: Stream[String], printer: ActorRef): String = {
    val interesting = i.map(x => x -> Utils.fastMD5(x)).filter(_._2.matches("^00000[0-7].*"))

    def findCode(tuples: Stream[(String, String)], code: String = "--------"): String = {
      if (!code.contains('-')) code
      else {
//        println(tuples.head)
        val hash = tuples.head._2
        val pos = hash(5).asDigit
        if (code(pos) == '-') {
          val newCode = code.updated(pos, hash(6))
          printer ! newCode
          if (!newCode.contains('-')) code
          else findCode(tuples.tail, newCode)
        } else {
          findCode(tuples.tail, code)
        }
      }
    }
    findCode(interesting).mkString
  }

//  val system = ActorSystem("printer")
//  val printer = system.actorOf(Props[PrintActor])
//  system.scheduler.schedule(.5 seconds, .5 seconds, printer, Rotate)
  val start = System.currentTimeMillis()
  val inputIndexes = Stream.from(0).map("wtnhxymk" + _)
//  solution2(inputIndexes, printer)
  println(solution1(inputIndexes))
//  system.terminate()
  println(s"Time consumed: ${System.currentTimeMillis() - start}ms")
}

case object Rotate

class PrintActor extends Actor {
  var current = "--------"
  override def receive = {
    case s: String =>
      current = s
      self ! Rotate
    case Rotate =>
      current = current.map(nextChar)
      print(s"\r${current}")
  }

  private def nextChar(c: Char): Char = c match {
    case '|' => '/'
    case '/' => '-'
    case '-' => '\\'
    case '\\' => '|'
    case x => x
  }
}

class SumActor extends Actor {
  var solution = ""
  override def receive = {
    case c: Char =>
      solution = solution + c
      if (solution.length == 8) println(solution)
  }
}

class MD5Actor(sumActor: ActorRef) extends Actor {
  override def receive: Receive = {
    case str: String =>
      val hash = md5(str)
//      println(hash)
      if (hash.startsWith("00000")) {
        println(hash(5))
        sumActor ! hash(5)
      }
  }
//  val digester = MessageDigest.getInstance("MD5")
  com.twmacinta.util.MD5.initNativeLibrary(true)

  def md5(s: String): String = {
    val hasher = new MD5()
    hasher.Update(s, null)
    hasher.asHex
//    Hex.valueOf(digester.digest(s.getBytes)).toLowerCase
  }
}
