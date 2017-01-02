package util

sealed trait Direction {
  def right: Direction
  def left: Direction
}

object Direction {
  case object Up extends Direction {
    def right = Right
    def left = Left
  }
  case object Down extends Direction {
    def right = Left
    def left = Right
  }
  case object Left extends Direction {
    def right = Up
    def left = Down
  }
  case object Right extends Direction {
    override def right: Direction = Down
    override def left: Direction = Up
  }
}
