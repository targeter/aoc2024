package aoc2024

import shared._

case object Day08 extends AocTools(day = 8) {
  type Grid = Map[Pos, Char]

  case class Pos(x: Int, y: Int) {
    def -(other: Pos): Pos = Pos(x - other.x, y - other.y)
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
  }

  def parse(input: Seq[String]): Grid = {
    input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (c, x) =>
        Pos(x, y) -> c
      }
    }.toMap
  }

  def antinodes(antennas: Seq[Pos]): Set[Pos] = {
    val nodes = for {
      a   <- antennas
      b   <- antennas
      if a != b
      diff = b - a
    } yield {
      Seq(a - diff, b + diff)
    }

    nodes.flatten.toSet
  }

  def antinodes2(grid: Grid, antennas: Seq[Pos]): Set[Pos] = {
    val nodes = for {
      a   <- antennas
      b   <- antennas
      if a != b
    } yield {
      val diff = b - a
      LazyList.iterate(a)(_ - diff).takeWhile(grid.contains) ++
      LazyList.iterate(b)(_ + diff).takeWhile(grid.contains)
    }
    nodes.flatten.toSet

  }

  def step1(input: Seq[String]): Int = {
    val grid        = parse(input)
    val antennaSets = grid.filterNot(_._2 == '.').groupMap(_._2)(_._1).values.map(_.toSeq)
    val all         = antennaSets.flatMap(antinodes).toSet
    all.count(grid.contains)
  }

  def step2(grid: Grid): Int = {
    val antennaSets = grid.filterNot(_._2 == '.').groupMap(_._2)(_._1).values.map(_.toSeq)
    antennaSets.flatMap(antinodes2(grid, _)).toSet.count(grid.contains)
  }

  def main(args: Array[String]): Unit = {
//    val grid = parse(lines)
//    println("Step 1: " + step1(inputLines))
//    println("Step 2: " + step2(inputLines))
  }
}
