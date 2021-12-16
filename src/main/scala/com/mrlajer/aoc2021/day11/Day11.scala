package com.mrlajer.aoc2021.day11

import com.mrlajer.aoc2021.utils.FileUtils

import scala.collection.mutable

object Day11 extends App {

  val input = FileUtils.ReadFile("day11.txt")

  val octopuses = input.map( l => l.toCharArray.map(_.asDigit))

  val width = octopuses.head.length
  val length = octopuses.length

  val octoMap: mutable.Map[(Int, Int), Octopus] = mutable.Map[(Int, Int), Octopus]()
  octopuses.indices.foreach(i => octopuses.head.indices.foreach(j => octoMap.addOne(((i,j), new Octopus(octopuses(i)(j), i, j, octoMap)))))

  var flashes = 0

  var hasAllFlashed = false
  var idx = 0
  while(!hasAllFlashed) {
    idx += 1
    octopuses.indices.foreach( i =>
      octopuses.head.indices.foreach{ j =>
        octoMap((i,j)).increase()
      }
    )
    octopuses.indices.foreach( i =>
      octopuses.head.indices.foreach { j =>
        val cur = octoMap((i, j))
        if (cur.shouldFlash()) cur.flash()
      }
    )
    if(octopuses.indices.forall( i =>
      octopuses.head.indices.forall{ j =>
        val cur = octoMap((i,j))
        cur.hasFlashed
      }
    )) {
      hasAllFlashed = true
      println(s"step: $idx")
    }
    octopuses.indices.foreach( i =>
      octopuses.head.indices.foreach{ j =>
        val cur = octoMap((i,j))
        if(cur.hasFlashed)
          cur.reset()
          flashes += 1
      }
    )

  }

  println(flashes)
}

class Octopus(var level: Int, val i: Int, val j: Int, val octoMap: mutable.Map[(Int, Int), Octopus], var hasFlashed: Boolean = false) {
  def increase(): Unit = level += 1
  def shouldFlash(): Boolean = !hasFlashed && level > 9
  def reset(): Unit =
    level = 0
    hasFlashed = false

  def flash(): Unit = {
    hasFlashed = true
    octoMap.get((i-1, j-1)).foreach(_.increase())
    octoMap.get((i-1, j)).foreach(_.increase())
    octoMap.get((i-1, j+1)).foreach(_.increase())
    octoMap.get((i, j-1)).foreach(_.increase())
    octoMap.get((i, j+1)).foreach(_.increase())
    octoMap.get((i+1, j-1)).foreach(_.increase())
    octoMap.get((i+1, j)).foreach(_.increase())
    octoMap.get((i+1, j+1)).foreach(_.increase())

    octoMap.get((i-1, j-1)).foreach(o => if(o.shouldFlash()) o.flash())
    octoMap.get((i-1, j)).foreach(o => if(o.shouldFlash()) o.flash())
    octoMap.get((i-1, j+1)).foreach(o => if(o.shouldFlash()) o.flash())
    octoMap.get((i, j-1)).foreach(o => if(o.shouldFlash()) o.flash())
    octoMap.get((i, j+1)).foreach(o => if(o.shouldFlash()) o.flash())
    octoMap.get((i+1, j-1)).foreach(o => if(o.shouldFlash()) o.flash())
    octoMap.get((i+1, j)).foreach(o => if(o.shouldFlash()) o.flash())
    octoMap.get((i+1, j+1)).foreach(o => if(o.shouldFlash()) o.flash())
  }


}
