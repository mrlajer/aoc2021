package com.mrlajer.aoc2021.day06

import com.mrlajer.aoc2021.utils.FileUtils

import scala.collection.mutable.ArrayBuffer

object Day06 extends App {

  val input = FileUtils.ReadFile("day06.txt")

  val fishes = input.head.split(",").map(_.toByte)

  var endFishes = new ArrayBuffer().addAll(fishes.foldLeft[Array[Long]](Array.ofDim(9)) { (a, f) =>
    a(f) += 1
    a
  })

  println(endFishes.mkString("Array(", ", ", ")"))

  (0 until 256).foreach { i =>
    val newFishes = new ArrayBuffer[Long]().addAll(endFishes)
    (endFishes.length-1 to 0 by -1).foreach { num =>

      val numFish = endFishes(num)
      if ( num == 0 ) {
        newFishes(8) += numFish
        newFishes(6) += numFish
        newFishes(0) -= numFish
      }
      else {
        newFishes(num) -= numFish
        newFishes(num -1) += numFish
      }


    }
    endFishes = newFishes

    println(s" $i -> ${endFishes.sum}")
  }




}
