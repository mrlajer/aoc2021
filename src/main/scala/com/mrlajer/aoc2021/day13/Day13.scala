package com.mrlajer.aoc2021.day13

import com.mrlajer.aoc2021.utils.FileUtils

object Day13 extends App {

  val input = FileUtils.ReadFile("day13.txt")

  val (coords, comms) = input.filter(_.nonEmpty).partition { l => l.nonEmpty && l.head.isDigit }

  coords.foreach(println)

  val commands = comms.map{ c => c.substring(c.lastIndexOf(' ')).split("=").map(_.trim) }

  val coordsMap = coords.map { l =>
    val splitted = l.split(",")
    splitted(0).toInt -> splitted(1).toInt
  }

  val width = coordsMap.map(_._1).max +1
  val length = coordsMap.map(_._2).max +1

  val paper = Array.fill[Array[Char]](length)(Array.ofDim[Char](width))

  coordsMap.foreach { case (x, y) => paper(y)(x) = '#' }

  def printOut(a: Array[Array[Char]]): Unit = {
    a.indices.foreach { r =>
      a.head.indices.foreach { l =>
        print(a(r)(l))
      }
      println
    }
  }

  def merge(a: Array[Char], b: Array[Char]): Array[Char] = {
    val res = Array.ofDim[Char](a.length)
    val r = a.zip(b).map { case (i,j) =>
      if(i == '#' || j == '#') '#'
      else '.'
    }
    r
  }

  def foldUp(a: Array[Array[Char]], line: Int): Array[Array[Char]] = {
    val length = a.length
    val half = length / 2
    val p = if ( line < half ) {
      Array.fill[Array[Char]](length-line)(Array.ofDim[Char](a.head.length)) ++ a
    } else a

    (0 until p.length-line-1).map { i =>
      merge(p(i), p(p.length-i-1))
    }.toArray
  }

  def count(a: Array[Array[Char]]): Int = {
    var count = 0
    a.indices.foreach { r =>
      a.head.indices.foreach { l =>
        if(a(r)(l) == '#') count += 1
      }
    }
    count
  }

  val res = commands.foldLeft[Array[Array[Char]]](paper) { (a, c) =>
    val axis = c.head
    val line = c.last.toInt
    if(axis == "x") foldUp(a.transpose, line).transpose
    else foldUp(a, line)
  }

  printOut(res)

  print(count(res))

}
