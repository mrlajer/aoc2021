package com.mrlajer.aoc2021.model

import com.mrlajer.aoc2021.model.Coord

case class Pair(c1: Coord, c2: Coord) {
  def maxX: Int = Math.max(c1.x, c2.x)
  def maxY: Int = Math.max(c1.y, c2.y)

  def isVertical: Boolean = c1.x == c2.x
  def isHorizontal: Boolean = c1.y == c2.y
  def isDiagonal: Boolean = Math.abs(c1.x - c2.x) == Math.abs(c1.y - c2.y)

  def isUp: Boolean = c1.y > c2.y
}

object Pair {
  def fromCoords(c: Array[Coord]): Pair = Pair(c(0), c(1))

  def min(p: Pair): Coord = if(p.c1.x < p.c2.x) p.c1 else p.c2
  def max(p: Pair): Coord = if(p.c1.x < p.c2.x) p.c2 else p.c1
}