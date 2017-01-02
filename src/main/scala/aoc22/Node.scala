package aoc22

/**
  * Created by jesper on 2016-12-29.
  */
case class Node(name: String, pos: Pos, used: Int, avail: Int) {
  override def toString = s"${padBefore(pos.toString, 7)}: ${padBefore(used.toString, 3)}T/${used+avail}T".padTo(9, ' ')
  private def padBefore(s: String, n: Int) = (1 to n - s.length).map(_ => ' ').mkString + s
}
