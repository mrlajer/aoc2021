package com.mrlajer.aoc2021.day02

import com.mrlajer.aoc2021.utils.FileUtils

object Day02 extends App {

  val input = FileUtils.ReadFile("day02.txt")

  def printPart1(): Unit = {

    val (horizontal, depth) = input.foldLeft((0, 0)) { case ((h, d), line) =>
      val (command, value) = {
        val array = line.split(" ")
        (array(0), array(1).toInt)
      }

      command match {
        case "forward" => (h + value, d)
        case "down" => (h, d + value)
        case "up" => (h, d - value)
      }
    }

    println(horizontal * depth)

  }

  printPart1()

  def printPart2(): Unit = {

    val (horizontal, depth, aim) = input.foldLeft((0, 0, 0)) { case ((h, d, a), line) =>
      val (command, value) = {
        val array = line.split(" ")
        (array(0), array(1).toInt)
      }

      command match {
        case "forward" => if (a == 0) (h + value, d, a) else (h + value, d + a * value, a)
        case "down" => (h, d, a + value)
        case "up" => (h, d, a - value)
      }
    }

    println(horizontal * depth)
  }

  printPart2()
}
