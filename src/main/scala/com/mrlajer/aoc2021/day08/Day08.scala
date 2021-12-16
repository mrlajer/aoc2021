package com.mrlajer.aoc2021.day08

import com.mrlajer.aoc2021.utils.FileUtils

object Day08 extends App {

  val input = FileUtils.ReadFile("day08.txt")

  val codeAndDigits = input.map(_.split("\\|"))
  val digits = codeAndDigits.map(r => r(1).trim.split(" "))
  val code = codeAndDigits.map(r => r(0).trim.split(" "))

  digits.foreach{row => println(row.mkString(" ")) }

  val result = digits.foldLeft(0){ (sum, d) =>
    sum + d.count(s => Set(2,3,4,7).contains(s.length))
  }

  println(result)

  def decode(s: Array[String]): Array[String] = {
    val res = Array.ofDim[String](10)

    res(1) = s.find(_.length == 2).mkString
    res(8) = s.find(_.length == 7).mkString
    res(7) = s.find(_.length == 3).mkString
    res(4) = s.find(_.length == 4).mkString

    val a = res(7).diff(res(1))

    res(9) = s.collect { case n if n.length == 6 && res(4).forall(c => n.contains(c)) && n.contains(a) => n}.mkString

    res(3) = s.collect { case n if n.length == 5 && res(1).forall(c => n.contains(c)) => n}.mkString

    val g = res(9).diff(res(4)).diff(a)
    val d = res(3).diff(res(1)).diff(a).diff(g)
    val b = res(4).diff(res(1)).diff(d)

    res(5) = s.collect { case n if n.length == 5 && n.contains(b) => n }.mkString

    val e = {
      val lll = s.collect { case n if n.length == 6 => n }
      val rrr = lll.diff(List(res(9), d))
      rrr.head.diff(res(9))
    }

    res(2) = s.collect { case n if n.length == 5 && n.contains(e) => n }.mkString

    val f = res(5).diff(a).diff(b).diff(d).diff(g)
    val c = res(1).diff(f)

    res(0) = s.collect { case n if n.length == 6 && n.contains(c) && n.contains(e) => n }.mkString
    res(6) = s.collect { case n if n.length == 6 && n.contains(d) && n.contains(e) => n }.mkString

    println(res.mkString(", "))
    res
  }

  val sum = code.zip(digits).foldLeft(0) { case (s, (c, d)) =>
    val nums = decode(c)

    val subsum =
      nums.indexOf(nums.find(n => n.length == d(0).length && d(0).forall(e => n.contains(e))).mkString) * 1000 +
        nums.indexOf(nums.find(n => n.length == d(1).length && d(1).forall(e => n.contains(e))).mkString) * 100 +
          nums.indexOf(nums.find(n => n.length == d(2).length && d(2).forall(e => n.contains(e))).mkString) * 10 +
            nums.indexOf(nums.find(n => n.length == d(3).length && d(3).forall(e => n.contains(e))).mkString)

    println(s"${d.mkString(" ")} - $subsum")
    s + subsum

  }

  println(sum)

}
