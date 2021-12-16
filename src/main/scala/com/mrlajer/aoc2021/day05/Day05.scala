package com.mrlajer.aoc2021.day05

import com.mrlajer.aoc2021.model.{Coord, Pair}
import com.mrlajer.aoc2021.utils.FileUtils

object Day05 extends App {

  val input = FileUtils.ReadFile("day05.txt")

  val lineCoords = input.flatMap(_.split(" -> ")
    .map(Coord.fromInput)
    .grouped(2)
    .map(Pair.fromCoords))

  lineCoords.foreach { l => print(s"$l - ${l.isDiagonal}) \n") }

  val (xMax, yMax) = lineCoords.foldLeft((0, 0)) { case ((x, y), p) =>
    (Math.max(x, p.maxX), Math.max(y, p.maxY))
  }

  val board = Array.fill(xMax + 1)(Array.fill[Int](yMax + 1)(0))

  var max = 0

  lineCoords.foreach { pair =>
    if (pair.isHorizontal) {
      val y = pair.c1.y
      val x1 = Math.min(pair.c1.x, pair.c2.x)
      val x2 = Math.max(pair.c1.x, pair.c2.x)
      (x1 to x2).foreach { x =>
        board(y)(x) += 1
        max = Math.max(max, board(y)(x))
      }
    } else if (pair.isVertical) {
      val x = pair.c1.x
      val y1 = Math.min(pair.c1.y, pair.c2.y)
      val y2 = Math.max(pair.c1.y, pair.c2.y)
      (y1 to y2).foreach { y =>
        board(y)(x) += 1
        max = Math.max(max, board(y)(x))
      }
    } else if (pair.isDiagonal) {
      val startCoord = Pair.min(pair)
      val endCoord = Pair.max(pair)

      println(s"diagonal pair: ${pair.c1} - ${pair.c2}")

      if (Pair(startCoord, endCoord).isUp) {
        val newCoords = (startCoord.x to endCoord.x).zip((startCoord.y to endCoord.y by -1)).map { case (a, b) => Coord(a, b) }
        newCoords.foreach { c =>
          println(s"newCoord: ${c}")
          board(c.y)(c.x) += 1 }
      } else {
        val newCoords = (startCoord.x to endCoord.x).zip((startCoord.y to endCoord.y)).map { case (a, b) => Coord(a, b) }
        newCoords.foreach { c =>
          println(s"newCoord: ${c}")
          board (c.y) (c.x) += 1
        }
      }
    }

  }


  board.foreach { row =>
    row.foreach(r => print(r))
    println()
  }

  val num = board.foldLeft(0) { (n, r) => n + r.count(_ > 1) }

  println(num)
}

