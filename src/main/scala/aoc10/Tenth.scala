package aoc10

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import util.Utils
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Tenth extends App {
  val givePattern = "bot (\\d+) gives low to (\\w+ \\d+) and high to (\\w+ \\d+)".r
  val goesPattern = "value (\\d+) goes to bot (\\d+)".r
  val input = Utils.load("/input10.txt", getClass)


  var bots = Map.empty[String, ActorRef]

  val system = ActorSystem("TenSystem")

  val multiplyer = system.actorOf(Props[MultiplyActor])

  input.foreach {
    case msg@givePattern(botNr, low, high) =>
      createOutputs(Seq(low, high))
      getOrCreate(botNr) ! GiveRules(low, high)
    case goesPattern(value, botNr) =>
      getOrCreate(botNr) ! Give(value.toInt)
    case x => {
      println(s"Oops: $x")
    }
  }

  //Yeah, I know its ugly, but works on my computer... :)
  system.scheduler.scheduleOnce(1 seconds, multiplyer, PrintVal)
  def getOrCreate(botNr: String): ActorRef = {
    val botName = s"bot-$botNr"
    bots.getOrElse(botName, {
      val actor = system.actorOf(Props(new BotActor(botNr.toInt)), name = botName)
      bots = bots.updated(botName, actor)
      actor
    })
  }

  def createOutputs(outputs: Seq[String]): Unit = {
    val pattern = "output (\\d+)".r
    outputs.filter(_.startsWith("output")).foreach {
      case pattern(nr) =>
        val botName = s"output-$nr"
        bots.getOrElse(botName, {
          val actor = system.actorOf(Props(new OutputActor(nr.toInt, multiplyer)), name = botName)
          bots = bots.updated(botName, actor)
        })
    }
  }
}

case class Give(value: Int)
case class GiveRules(low: String, high: String)

class OutputActor(nr: Int, multiplyer: ActorRef) extends Actor {
  var chips = Set.empty[Int]
  def receive = {
    case Give(v) =>
      chips = chips + v
      if (nr < 3) multiplyer ! v
  }
}

case object PrintVal
class MultiplyActor extends Actor {
  var x = 1
  def receive = {
    case y: Int =>
      x *= y
    case PrintVal =>
      println(s"Answer 2: $x")
  }
}

class BotActor(nr: Int) extends Actor {
  var chips = Set.empty[Int]
  var low: Option[String] = None
  var high: Option[String] = None

  def receive = {
    case Give(v) =>
      chips = chips + v
      sendChips
    case GiveRules(l, h) =>
      low = Some(l)
      high = Some(h)
      sendChips
  }
  private def sendChips: Unit = {
    if (chips.size == 2) {
      for {
        l <- low.map(_.replace(' ', '-'))
        h <- high.map(_.replace(' ', '-'))
      } yield {
        val min = chips.min
        val max = chips.max
        if (chips == Set(17, 61)) println(s"Answer: $nr")
        chips = chips - min - max
        context.actorSelection(s"../$l") ! Give(min)
        context.actorSelection(s"../$h") ! Give(max)
      }
    }
  }
}
