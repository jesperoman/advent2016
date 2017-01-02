package aoc11

sealed trait Device {
  val subject: String
}

object Device {
  case class Generator(override val subject: String) extends Device {
    override def toString = s"G${subject.take(2).toUpperCase}"
  }
  case class MicroChip(override val subject: String) extends Device {
    override def toString = s"M${subject.take(2).toUpperCase}"
  }
}
