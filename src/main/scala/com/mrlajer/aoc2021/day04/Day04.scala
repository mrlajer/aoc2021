package com.mrlajer.aoc2021.day04

import com.mrlajer.aoc2021.model.{Board, Markable}
import com.mrlajer.aoc2021.utils.FileUtils

import scala.annotation.tailrec

object Day04 extends App {

  val input = FileUtils.ReadFile("day04.txt").mkString("\n")

  val splitted = input.split("\n\n")

  val numbers = splitted(0).split(",").map(_.toInt).toList

  val startingBoards =
    splitted.tail
      .map(board => board.split("\n")).toList
      .map(_.map(e => e.trim.split(" ").filter(_ != "")))
      .map(_.map(_.map(n => Markable(n.toInt))))


  val boardsWithIndex = startingBoards.zipWithIndex.map{ case (b, idx) => new Board(b, idx) }

  def check(b: Board): Boolean = {
    def checkArray(a: Array[Array[Markable]]): Boolean = a.exists( l => l.forall(_.isMarked))

    checkArray(b.b) || checkArray(b.b.transpose)
  }

  var counter = 0

  @tailrec
  def loop(nums: List[Int], boards: List[Board]): (Board, Int) = {
    @tailrec
    def markLoop(bs: List[Board]): Unit = {
        if ( bs.nonEmpty ) {
          val b = bs.head
          b.b.foreach(_.foreach{n =>
            if (n.num == nums.head) {
              n.mark()
            }
          })

          if (!b.hasWon && check(b)) {
            b.won()
            counter += 1
            if ( counter == startingBoards.size )
              println(s"last winner: ${b.id}, num: ${nums.head}, result: ${b.b.map(_.filterNot(_.isMarked).map(_.num).sum).sum * nums.head}")
          }
          markLoop(bs.tail)
        }
    }

    markLoop(boards)

    if ( nums.size == 1 ) (boards.head, nums.head)
    else loop(nums.tail, boards)
  }

  val (winner, lastNum) = loop(numbers, boardsWithIndex)

  println(winner.sumUnmarked() * lastNum)


}
