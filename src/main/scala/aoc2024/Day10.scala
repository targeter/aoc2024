package aoc2024

import shared._

case object Day10 extends AocTools(day = 10) {
  def parse(lines: Seq[String]): Seq[Seq[Int]] = lines.map(_.map(_.asDigit))

  case class Pos(x: Int, y: Int)

  implicit class GridOps(grid: Seq[Seq[Int]]) {
    val positions: Seq[Pos] = for {
      y <- 0 until grid.size
      x <- 0 until grid(y).size
    } yield Pos(x, y)

    def apply(pos: Pos) = grid(pos.y)(pos.x)

    def allWhere(f: Int => Boolean): Seq[Pos] = positions.filter(p => f(apply(p)))

    def get(pos: Pos): Int = grid(pos.y)(pos.x)

    def around(pos: Pos): Seq[Pos] = {
      Seq(pos.copy(x = pos.x - 1), pos.copy(x = pos.x + 1), pos.copy(y = pos.y - 1), pos.copy(y = pos.y + 1))
        .filter(p => p.x >= 0 && p.x < grid.head.size && p.y >= 0 && p.y < grid.size && p != pos)
    }
  }

  def nextSteps1(grid: Seq[Seq[Int]], curr: Int, pos: Pos): Seq[Pos] = {
    if (curr == 9) Seq(pos)
    else {
      val nexts = grid.around(pos).filter(p => grid.get(p) == curr + 1)
      if (nexts.isEmpty) Seq.empty[Pos]
      else nexts.flatMap(n => nextSteps1(grid, curr + 1, n))
    }
  }

  def nextSteps2(grid: Seq[Seq[Int]], curr: Int, pos: Pos): Int = {
    if (curr == 9) 1
    else {
      val nexts = grid.around(pos).filter(p => grid.get(p) == curr + 1)
      if (nexts.isEmpty) 0
      else nexts.map(n => nextSteps2(grid, curr + 1, n)).sum
    }
  }

  def step1(lines: Seq[String]): Int = {
    val grid = parse(lines)
    grid
      .allWhere(_ == 0)
      .map(nextSteps2(grid, 0, _))
      .sum
  }

  def step2(lines: Seq[String]): Int = {
    val grid = parse(lines)
    grid
      .allWhere(_ == 0)
      .map(nextSteps1(grid, 0, _))
      .map(_.distinct.size)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines))
    println("Step 2: " + step2(inputLines))
  }
}
