package com.mrlajer.aoc2021.day09

import com.mrlajer.aoc2021.model.Markable
import com.mrlajer.aoc2021.utils.FileUtils

import scala.annotation.tailrec

object Day09 extends App {

  val input = FileUtils.ReadFile("day09.txt")

  val map = input.map( row => row.toCharArray.map(c => new Markable(c.asDigit))).toArray

  val width = map(0).length
  val length = map.length

  def isMin(i: Int, j: Int): (Boolean, Int) = {
    val current = map(i)(j)

    (
      (if(j > 0) current < map(i)(j-1) else true) &&
        (if(j < width -1) current < map(i)(j+1) else true) &&
        (if(i > 0) current < map(i-1)(j) else true) &&
        (if(i < length -1) current < map(i+1)(j) else true),
      current.num
    )
  }

  @tailrec
  def rowLoop(i: Int, min: Int): Int = {
    @tailrec
    def colLoop(j: Int, m: Int): Int = {
      if (j == width) m
      else {
        val (min, current) = isMin(i, j)
        if (min) colLoop(j+1, m + current + 1)
        else colLoop(j + 1, m)
      }
    }
    if(i == length) min
    else {
      val rowMin = colLoop(0, min)
      rowLoop(i+1, rowMin)
    }
  }

  val minSum = rowLoop(0, 0)

  println(s"minSum: $minSum")


  val numOfBasins: Iterable[Int] =
    map.indices.flatMap{ i =>
      map(i).indices.map{ j =>

        def markLoop(k: Int, l: Int, size: Int): Int = {
          if (k < 0 || l < 0 || k == length || l == width) size
          else {
            val c = map(k)(l)
            if (c.isMarked) size
            else {
              c.mark()
              if (c.num == 9) size
              else {
                val s1 = markLoop(k, l - 1, size +1)
                val s2 = markLoop(k + 1, l, s1)
                val s3 = markLoop(k-1,l, s2)
                markLoop(k, l + 1, s3)
              }
            }
          }
        }
        val current = map(i)(j)
        if (!current.isMarked) markLoop(i, j, 0) else 0
      }
    }

  val filteredList = numOfBasins.filterNot(_ == 0).toList.sorted.reverse
  println(filteredList.take(3).product)


}
