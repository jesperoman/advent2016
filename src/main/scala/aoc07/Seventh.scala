package aoc07

import util.Utils

object Seventh extends App {

  val ips = input.map(_.trim)

  val pattern = "([a-z]+\\[|\\][a-z]+\\[|\\][a-z]+)".r

  val brackets = """\[[a-z]+\]""".r

  val abbas = ips.count { x =>
    val hypernet = brackets.findAllIn(x).toList
    val ip = pattern.findAllIn(x).toList
    hypernet.forall(a => !containsABBA(a)) && ip.exists(containsABBA)
  }

  println(abbas)

  val abas = ips.count { x =>
    val hypernet = brackets.findAllIn(x).toList
    val ip = pattern.findAllIn(x).toList

    val invertedABAS = ip.foldLeft(Set.empty[String]) (_ ++ findABAs(_)).map(invertABA)

    hypernet.exists(addr => invertedABAS.exists(addr.contains))
  }

  def invertABA(s: String) = {
    s(1).toString + s(0).toString + s(1).toString
  }

  println(abas)
  def containsABBA(s: String): Boolean = s match {
    case "" => false
    case chars if chars.length < 4 => false
    case chars if chars(0) != chars(1) && chars.take(2) == chars.slice(2, 4).reverse => true
    case chars => containsABBA(chars.tail)
  }

  def findABAs(s: String, acc: Set[String] = Set.empty): Set[String] = {
    val pattern = "^((.).\\2).*".r
    s match {
      case "" => acc
      case pattern(aba, _) => findABAs(s.tail, acc + aba)
      case _ => findABAs(s.tail, acc)
    }
  }


//  println(ips.take(5).mkString("\n"))
  lazy val input = Utils.load("/input7.txt", getClass)
}
