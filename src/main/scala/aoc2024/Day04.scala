package aoc2024

import shared._

import scala.util.Try

case object Day04 extends AocTools(day = 4) {
  def coords(lines: Seq[Seq[Char]]): Seq[(Int, Int)] = for {
    y <- lines.indices
    x <- lines(y).indices
  } yield (x, y)

  val deltas: Seq[(Int, Int)] = for {
    dy <- -1 to 1
    dx <- -1 to 1
    if dx != 0 || dy != 0
  } yield dy -> dx

  def move(point: (Int, Int), move: (Int, Int), times: Int): (Int, Int) = (point._1 + (move._1 * times), point._2 + (move._2 * times))

  def onGrid(grid: Seq[Seq[Char]], p: (Int, Int)): Option[Char] = Try(grid(p._1)(p._2)).toOption

  def words(p: (Int, Int), grid: Seq[Seq[Char]]) =
    deltas.foldLeft(List[String]())((acc, d) => acc :+ (p +: (1 to 3).map(move(p, d, _))).flatMap(onGrid(grid, _)).mkString)

  def check(grid: Seq[Seq[Char]], p: (Int, Int)): Boolean = {
    val cross = Seq(
      Seq((p._1 - 1, p._2 - 1), (p._1 + 1, p._2 + 1)),
      Seq((p._1 + 1, p._2 + 1), (p._1 - 1, p._2 - 1)),
      Seq((p._1 + 1, p._2 - 1), (p._1 - 1, p._2 + 1)),
      Seq((p._1 - 1, p._2 + 1), (p._1 + 1, p._2 - 1))
    )
    cross.map(_.flatMap(onGrid(grid, _)).mkString).count(_ == "MS") >= 2
  }

  def step1(grid: Seq[Seq[Char]]): Int = coords(grid).flatMap(words(_, grid)).count(_ == "XMAS")
  def step2(grid: Seq[Seq[Char]]): Int = {
    coords(grid)
      .filter(c => onGrid(grid, c).contains('A'))
      .count(check(grid, _))
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines.map(_.toSeq))) // 2560
    println("Step 2: " + step2(inputLines.map(_.toSeq))) // 1910
  }
}
