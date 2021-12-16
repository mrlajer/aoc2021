package com.mrlajer.aoc2021.day10

import com.mrlajer.aoc2021.utils.FileUtils

import scala.annotation.tailrec
import scala.collection.mutable

object Day10 extends App {

  val input = FileUtils.ReadFile("day10.txt")

  val openingChars = "([{<".toSet

  def isMatch(ch1: Char, ch2: Char): Boolean = {
    val chs = Set(ch1, ch2)
    chs.equals(Set('(', ')')) ||
      chs.equals(Set('[', ']')) ||
      chs.equals(Set('{', '}')) ||
      chs.equals(Set('<', '>'))
  }

  @tailrec
  def parseLine(stack: Seq[Char], chars: Seq[Char]): Option[Char] = {
    if ( chars.isEmpty ) None
    else {
      val cur = chars.head
      if ( openingChars(cur) ) parseLine(stack :+ cur, chars.tail)
      else if(isMatch(stack.last, cur)) parseLine(stack.dropRight(1), chars.tail)
      else Some(cur)
    }
  }

  val points = Map(
    ')' -> 3L,
    ']' -> 57L,
    '}' -> 1197L,
    '>' -> 25137L
  )

  val firstIllegals = input.flatMap{ line => parseLine(Nil, line) }
  println(firstIllegals.mkString(", "))

  println(firstIllegals.map(c => points(c)).sum)

  val incompleteLines = input.filterNot(l => parseLine(Nil, l).isDefined)

  @tailrec
  def parseLine2(stack: Seq[Char], chars: Seq[Char]): Seq[Char] = {
    if ( chars.isEmpty ) stack
    else {
      val cur = chars.head
      if ( openingChars(cur) ) parseLine2(stack :+ cur, chars.tail)
      else if(isMatch(stack.last, cur)) parseLine2(stack.dropRight(1), chars.tail)
      else sys.error("wat")
    }
  }

  def closingFor(ch: Char): Char = ch match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    case _ => sys.error("wat")
  }

  val autoCompletePoints = Map(
    ')' -> 1L,
    ']' -> 2L,
    '}' -> 3L,
    '>' -> 4L
  )

  val autoCompleteClosing = incompleteLines.map(l => parseLine2(Nil, l))

  autoCompleteClosing.foreach(l => println(l.map(closingFor).reverse.mkString))

  val scores = autoCompleteClosing.map{ line =>
    line.map(closingFor).reverse.foldLeft(0L) { (acc, ch) => acc * 5L + autoCompletePoints(ch) }
  }.sorted.toIndexedSeq

  val score = scores(scores.length / 2)
  println(score)

}

case class Chunk(opening: Option[Char], closing: Option[Char], children: List[Chunk])