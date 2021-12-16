package com.mrlajer.aoc2021.day03

import com.mrlajer.aoc2021.utils.FileUtils
import scala.annotation.tailrec

object Day03 extends App {

  val input = FileUtils.ReadFile("day03.txt")

  val inputArray = input.map(_.toCharArray).toArray
  val transposed = inputArray.transpose.toList.map(_.toList)

  val max2power = transposed.length -1

  val (g, e, _) = transposed.foldLeft[(Double, Double, Int)]((0, 0, max2power)) { case ((gamma, epsilon, p), line) =>
    val (zero, one) = line.partition(_ == '0')
    if (zero.length > one.length) (gamma, epsilon + Math.pow(2, p), p-1) else (gamma + Math.pow(2, p), epsilon, p-1)
  }

  println(s"gamma * epsilon: ${(g * e).toInt}")

  val o2 = {
    @tailrec
    def loop(l: List[List[Char]], idx: Int): List[Char] = {
      val line = l.take(idx+1).last
      val (zero, one) = line.partition(_ == '0')

      val o2Filter: List[Char] => Boolean = ch => if(zero.length > one.length) ch(idx) == '0' else ch(idx) == '1'
      val o2 = l.transpose.filter(o2Filter)

      if (o2.length == 1) o2.head
      else loop(o2.transpose, idx+1)
    }

    loop(transposed, 0)
  }

  println(o2)

  val co2 = {
    @tailrec
    def loop(l: List[List[Char]], idx: Int): List[Char] = {
      val line = l.take(idx + 1).last
      val (zero, one) = line.partition(_ == '0')

      val co2Filter: List[Char] => Boolean = ch => if(zero.length > one.length) ch(idx) == '1' else ch(idx) == '0'
      val co2 = l.transpose.filter(co2Filter)

      if (co2.length == 1) co2.head
      else loop(co2.transpose, idx + 1)
    }

    loop(transposed, 0)
  }

  println(co2)

  def toDecimal(binary: List[Char]): Double = {
    val max2pow = binary.length - 1

    val (res, _) = binary.foldLeft((0d, max2pow)) { case ((dec, p), ch) =>
      (dec + (ch.asDigit.toDouble * Math.pow(2, p)), p-1)
    }
    res
  }

  println((toDecimal(o2) * toDecimal(co2)).toInt)

}
