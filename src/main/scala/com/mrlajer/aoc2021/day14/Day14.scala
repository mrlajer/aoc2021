package com.mrlajer.aoc2021.day14

import com.mrlajer.aoc2021.utils.FileUtils

import scala.collection.mutable

object Day14 extends App {

  val input = FileUtils.ReadFile("day14.txt")

  val template = input.head

  val rules = input.tail.withFilter(_.nonEmpty).map(l => l.split(" -> ")).map(a => a(0) -> a(1)).toMap

  def step(str: String): String = str.sliding(2).foldLeft("") { (acc, pair) =>
    val res = s"${acc.dropRight(1)}${pair(0)}${rules.get(pair).mkString}${pair(1)}"
//    println(s"res: $res, acc: $acc, pair: $pair")
    res
  }

//  val polymer = (1 to 40).foldLeft(template){ (acc, i) =>
//    println(s"step $i")
//    step(acc)
//  }

//  println(polymer)

//  val counter: mutable.Map[Char, Long] = mutable.Map[Char, Long]()
//
//  polymer.foreach{ch =>
//    if(counter.contains(ch)) {
//      val curVal = counter.getOrElse(ch, 0L)
//      counter.update(ch, curVal +1L )
//    } else {
//      counter.addOne((ch, 1L))
//    }
//  }
//
//  val minValue = counter.values.min
//  val maxValue = counter.values.max
//
//  println(maxValue - minValue)

//  def step2(str: String) = {
//    (1 to 10).foldLeft(str) { (acc, _) =>
//      s"${acc.dropRight(1)}${str(0)}${rules.get(str).mkString}${str(1)}"
//    }
//  }
//
//
//  template.sliding(2).foldLeft("") { (acc, pair) =>
//    (1 to 10).foldLeft("") { (a, _) =>
//      s"${a.dropRight(1)}${pair(0)}${rules.get(pair).mkString}${pair(1)}"
//    }
//  }

  val pairs = mutable.Map[String, Long]()

  // init
  template.sliding(2).foreach { p =>
    val cur = pairs.getOrElse(p, 0L)
    pairs.addOne(p, cur + 1L)
  }

  // steps
  (1 to 40).foreach { i =>
    val existing = pairs.filterNot((k,v) => v == 0L)
    println(s"step $i, existing: ${existing.mkString(",")}")
    existing.foreach{(k,v) =>
      val middle = rules(k)
      val first = s"${k(0)}$middle"
      val second = s"$middle${k(1)}"
      pairs(k) = pairs(k) - v
      if(!pairs.contains(first)) pairs.addOne((first, v)) else pairs(first) = pairs(first) + v
      if(!pairs.contains(second)) pairs.addOne((second, v)) else pairs(second) = pairs(second) + v
    }
  }

  pairs.foreach(println)

  val cnt = pairs.foldLeft(mutable.Map[String, Long]()) { case (acc, (k, v)) =>
    if (acc.contains(k(0).toString)) acc(k(0).toString) = acc(k(0).toString) + v else acc.addOne((k(0).toString, v))
    if (acc.contains(k(1).toString)) acc(k(1).toString) = acc(k(1).toString) + v else acc.addOne((k(1).toString, v))
    acc
  }




  val minValue = cnt.values.min
  val maxValue = cnt.values.max

  println((maxValue - minValue)/2)













}
