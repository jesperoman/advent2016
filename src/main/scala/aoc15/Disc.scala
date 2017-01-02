package aoc15

/**
  * Created by jesper on 2016-12-29.
  */
case class Disc(nr: Int, positions: Int, current: Int) {
  def atTime(time: Int) = Disc(nr, positions, (time + nr + current) % positions)
}
