package com.mrlajer.aoc2021.day15

import com.mrlajer.aoc2021.day03.Day03.input
import com.mrlajer.aoc2021.utils.FileUtils

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day15 extends App {

  val input = FileUtils.ReadFile("day15.txt")

  val inputArray = input.map(_.toCharArray.map(_.asDigit)).toArray

  var min = Integer.MAX_VALUE;

  val width = inputArray.head.length
  val length = inputArray.length


  def loop(i: Int, j: Int, curVal: Int): Unit = {
    if(j == length && i == width) { if(curVal < min) min = curVal }
    else if(j == length && i < width) loop(i+1, j, curVal + inputArray(i)(j))
    else if(i == width && j < length) loop(i, j+1, curVal + inputArray(i)(j))
    else {
      loop(i, j+1, curVal + inputArray(i)(j))
      loop(i+1, j, curVal + inputArray(i)(j))
    }
  }



  val c = ArrayBuffer.fill(width)(ArrayBuffer.fill(length)(0))

  def minpath(x: Int, y: Int): Int = {
    c(0)(0) = inputArray(0)(0)

    (1 until width).foreach(i => c(i)(0) = c(i-1)(0) + inputArray(i)(0) )
    (1 until length).foreach(j => c(0)(j) = c(0)(j-1) + inputArray(0)(j) )

    (1 until width).foreach { i =>
      (1 until length).foreach { j =>
        c(i)(j) = Math.min(c(i-1)(j), c(i)(j-1))
          + inputArray(i)(j)
      }
    }

    c(width-1)(length-1) - c(0)(0)
  }

  println(minpath(width, length))

//  inputArray.foreach { l =>
//    l.foreach(e => print(s"$e "))
//    println
//  }
//
//  c.foreach { l =>
//    l.foreach(e => print(s"$e "))
//    println
//  }



}
