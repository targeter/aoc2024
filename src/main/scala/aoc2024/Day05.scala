package aoc2024

import shared._

case object Day05 extends AocTools(day = 5) {
  def parse(lines: Seq[String]): (Ordering[Int], Seq[Seq[Int]]) = {
    val orderInstructions = lines.takeWhile(_.nonEmpty)
    val updates = parseIntLines(lines.dropWhile(_.nonEmpty).tail)

    val ordering = orderInstructions.map {
      case s"$x|$y" => new UpdateOrdering(x.toInt, y.toInt)
    }.reduce[Ordering[Int]](_ orElse _)

    (ordering, updates)
  }

  def step1(lines: Seq[String]): Int = answer {
    val (ordering, updates) = parse(lines)
    updates.filter(l => l.sorted(ordering) == l)
  }

  def step2(lines: Seq[String]): Int = answer {
    val (ordering, updates) = parse(lines)

    updates
      .filterNot(l => l.sorted(ordering) == l)
      .map(_.sorted(ordering))
  }

  def answer(lines: Seq[Seq[Int]]): Int = lines.map(l => l(l.size / 2)).sum

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines))
    println("Step 2: " + step2(inputLines))
  }
}

class UpdateOrdering(first: Int, last: Int) extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = {
    (x, y) match {
      case (`first`, `last`) => -1
      case (`last`, `first`) => 1
      case _                 => 0
    }
  }
}
