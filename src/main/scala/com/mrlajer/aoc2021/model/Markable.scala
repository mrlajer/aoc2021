package com.mrlajer.aoc2021.model

class Markable(val num: Int) extends Comparable[Markable] with Ordered[Markable] {
  var isMarked: Boolean = false

  def mark(): Unit = isMarked = true

  override def compareTo(o: Markable): Int = num.compareTo(o.num)

  override def compare(that: Markable): Int = num.compare(that.num)
}