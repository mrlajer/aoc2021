package com.mrlajer.aoc2021.day17

import scala.annotation.tailrec

object Day17 extends App {

  val xmin = 57
  val xmax = 116
  val ymin = -198
  val ymax = -148

  def isInTarget(x: Int, y: Int, xmin: Int, xmax: Int, ymin: Int, ymax: Int): Boolean =
    x <= xmax && x >= xmin && y <= ymax && y >= ymin

  @tailrec
  def loop(x: Int, vx: Int, y: Int, vy: Int, ys: List[Int]): Option[Int] = {
    if(x > xmax || y < ymin) None
    else if(isInTarget(x, y, xmin, xmax, ymin, ymax)) ys.maxOption
    else {
      val newVx = if(vx == 0) 0 else if(vx < 0) vx + 1 else vx -1
      val newVy = vy -1
      loop(x + vx, newVx, y + vy, newVy, ys :+ y)
    }
  }

  val result: Seq[Int] = (0 to xmax).flatMap { vx =>
    (ymin to -ymin).flatMap { vy =>
      loop(0, vx, 0, vy, Nil)
    }
  }

  println(result.max) // part 1
  println(result.size) // part 2

}
