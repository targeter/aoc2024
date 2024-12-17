package aoc2024

import shared._

case object Day11 extends AocTools(day = 11) {

  def parse(input: String): Seq[Long] = input.split(" ").map(_.toLong).toSeq

  def blink(input: Long): Seq[Long] = input match {
    case 0 => Seq(1)
    case n if n.toString.length % 2 == 0 =>
      val (a, b) = n.toString.splitAt(n.toString.length / 2)
      Seq(a.toLong, b.toLong)
    case n => Seq(n * 2024)
  }

  def step1(lines: Seq[String], n: Int): Long = {
    val counts: Map[Long, Long] = parse(lines.head).groupBy(identity).view.mapValues(_.size.toLong).toMap

    def doSomething(c: Map[Long, Long]) = c.toSeq
      .flatMap { case (k, v) => blink(k).map(_ -> v) }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sum)
      .toMap

    LazyList.iterate(counts)(doSomething)(n).values.sum
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines, 25))
    println("Step 2: " + step1(inputLines, 75))
  }
}
