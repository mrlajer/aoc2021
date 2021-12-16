package com.mrlajer.aoc2021.model

case class Coord(x: Int, y: Int)

object Coord {
  def fromInput(str: String): Coord = {
    val elements = str.split(",")
    Coord(elements.head.toInt, elements.last.toInt)
  }
}