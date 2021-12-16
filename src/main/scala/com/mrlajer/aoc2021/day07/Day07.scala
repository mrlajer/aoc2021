package com.mrlajer.aoc2021.day07

import com.mrlajer.aoc2021.utils.FileUtils

object Day07 extends App {

  val input = FileUtils.ReadFile("day07.txt")
  val positions = input.head.split(",").map(_.toInt)
  val max = positions.max

  val distances1 = positions.map { p =>
    val d = Array.fill(max+1)(0)
    (p-1 to 0 by -1).foreach{ i => d(i) = d(i+1) + 1 }
    (p+1 to max).foreach{ i => d(i) = d(i-1) + 1 }
    d
  }

  val distances2 = positions.map { p =>
    val d = Array.fill(max+1)(0)
    (p-1 to 0 by -1).zipWithIndex.foreach{ case (i, s) => d(i) = d(i+1) + s + 1 }
    (p+1 to max).zipWithIndex.foreach{ case (i, s) => d(i) = d(i-1) + s + 1 }
    d
  }

  val min1 = distances1.transpose.map(_.sum).min
  val min2 = distances2.transpose.map(_.sum).min

  println(min1)
  println(min2)

}
