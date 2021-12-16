package com.mrlajer.aoc2021

import com.mrlajer.aoc2021.utils.FileUtils

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Day12 extends App {

  val input = FileUtils.ReadFile("day12.txt")

  def findCave(name: String, caves: Seq[Cave]): (Seq[Cave], Seq[Cave]) =
    val cur = caves.find(_.name == name)
    if ( cur.isEmpty )
      val newItem = Seq(Cave(name, Nil))
      (newItem, caves ++ newItem)
    else
      (cur.toSeq, caves)

  val caveList : Seq[Cave] = input.foldLeft[Seq[Cave]](Nil) { (acc, l) =>
    val edge = l.split("-").map(_.trim)
    val from = edge(0)
    val to = edge(1)

    val (caves, updatedAcc) = findCave(from, acc)
    val (conns, updatedAcc2) = findCave(to, updatedAcc)

    caves.foreach(_.addConnections(conns))
    conns.foreach(_.addConnections(caves))

    updatedAcc2
  }

//  caveList.foreach{ c => println(c) }

  def process(caves: Seq[Cave], cp: Seq[String], fp: Seq[Seq[String]]): Seq[Seq[String]] = {
    if(caves.isEmpty) fp
    else {
      val cave = caves.head

      val pathsFromThisCave =
        if ( cave.name == "end") fp :+ (cp :+ "end")
        else if (cave.isSmall && cp.contains(cave.name)) Nil
        else process(cave.connections, cp :+ cave.name, fp)

      pathsFromThisCave ++ process(caves.tail, cp, fp)
    }
  }

  val paths: Seq[Seq[String]] =
    process(caveList.find(_.name == "start").toSeq, Nil, Nil)

//  paths.foreach(p => println(p.mkString(", ")))

  println(paths.length)

  def containsTwo(caves: Seq[Cave]): Boolean = {
    caves.exists { cave => cave.isSmall && caves.collect { case c if c.name == cave.name => c }.size >= 2 }
  }

  def process2(caves: Seq[Cave], visitedCaves: Seq[Cave], fp: Seq[Seq[String]]): Seq[Seq[String]] = {
    if(caves.isEmpty) fp
    else {
      val cave = caves.head
      val pathsFromThisCave =
        if ( cave.name == "end") fp :+ (visitedCaves.map(_.name) :+ "end")
        else if (cave.isSmall && visitedCaves.contains(cave) && (cave.isStartOrEnd || containsTwo(visitedCaves))) Nil
        else process2(cave.connections, visitedCaves :+ cave, fp)
      pathsFromThisCave ++ process2(caves.tail, visitedCaves, fp)
    }
  }

  val paths2: Seq[Seq[String]] =
    process2(caveList.find(_.name == "start").toSeq, Nil, Nil)

//  paths2.foreach(p => println(p.mkString(", ")))

  println(paths2.length)

}

class Cave(val name: String, var connections: Seq[Cave]) {
  val isSmall: Boolean = name.forall(_.isLower)
  val isStartOrEnd: Boolean = name == "start" || name == "end"

  def addConnections(c: Seq[Cave]): Unit = connections ++= c

  override def toString: String = s"""$name(${connections.map(_.name).mkString(",")})"""
}
