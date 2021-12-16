package com.mrlajer.aoc2021.day01

import com.mrlajer.aoc2021.utils.FileUtils

object Day01 extends App {

  val input = FileUtils.ReadFile("day01.txt")

  val result1 = input.sliding(2).foldLeft(0){ (acc, l) => if(l.head.toInt < l.last.toInt) acc + 1 else acc }
  println(result1)


  val result2 = input.sliding(3).sliding(2).foldLeft(0){ (acc, l) => if(l.head.map(_.toInt).sum < l.last.map(_.toInt).sum) acc + 1 else acc }
  println(result2)

}
