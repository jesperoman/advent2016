package util

import java.security.MessageDigest

import com.twmacinta.util.MD5

object Utils {
  def load(fName: String, c: Class[_] = getClass): List[String] =
    scala.io.Source.fromInputStream(c.getResourceAsStream(fName)).getLines.toList

  val digester = MessageDigest.getInstance("md5")
  def standardMD5(x: String) = {
    digester.digest(x.getBytes).map("%02X" format _).mkString.toLowerCase
  }

  com.twmacinta.util.MD5.initNativeLibrary(true)
  def fastMD5(s: String): String = {
    val hasher = new MD5()
    hasher.Update(s, null)
    hasher.asHex
  }

  def fastMD5Bytes(s: String): Array[Byte] = {
    val hasher = new MD5()
    hasher.Update(s, null)
    hasher.Final()
  }

  def stretchedMD5(x: String, times: Int): String = {
    times match {
      case 0 => x
      case t => stretchedMD5(fastMD5(x), times - 1)
    }
  }

  def solve[A, B](
    start: A,
    goal: A => Boolean,
    possibleNexts: A => List[A],
    uniqueBy: A => B,
    debug: Boolean = false,
    best: List[A] => A = (l: List[A]) => l.head
  ): A = {
    def loop(states: List[A], explored: Set[B]): A = {
      if (debug) println(states.length)
      states.filter(goal) match {
        case Nil =>
          val newRoutes = states
            .flatMap(possibleNexts)
            .filterNot(a => explored(uniqueBy(a)))
            .groupBy(uniqueBy).toList.map(_._2.head)
          loop(newRoutes, explored ++ newRoutes.map(uniqueBy))
        case l => best(l)
      }
    }
    loop(List(start), Set(uniqueBy(start)))
  }


}
