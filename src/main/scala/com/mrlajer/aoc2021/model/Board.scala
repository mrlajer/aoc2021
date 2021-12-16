package com.mrlajer.aoc2021.model

class Board(val b: Array[Array[Markable]], val id: Int, var hasWon: Boolean = false) {

  def won(): Unit = hasWon = true

  def printStr(): Unit = b.foreach{ e =>

    e.foreach { m =>
      val markedSign = if (m.isMarked) "x" else ""
      print(s" $markedSign${m.num}") }
    println("\n")
  }

  def sumUnmarked(): Int = b.map(_.filterNot(_.isMarked).map(_.num).sum).sum
}