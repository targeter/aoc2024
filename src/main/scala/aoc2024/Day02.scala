package aoc2024

import shared._

case object Day02 extends AocTools(day = 2) {

  def inOrder(l: Seq[Int]): Boolean = l.sorted == l || l.sorted == l.reverse

  def gradual(l: Seq[Int]): Boolean = l
    .sliding(2)
    .forall(p => {
      val diff = (p.head - p(1)).abs
      diff > 0 && diff <= 3
    })

  def inOrderAndGradual(l: Seq[Int]): Boolean = inOrder(l) && gradual(l)

  def step1(lines: Seq[Seq[Int]]): Int = lines.count(inOrderAndGradual)

  def step2(lines: Seq[Seq[Int]]): Int = {
    lines.map { line =>
      line +: line.indices.map(i => line.patch(i, Nil, 1)).toList
    }.count(_.exists(inOrderAndGradual))
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLineInts(" "))) // 526
    println("Step 2: " + step2(inputLineInts(" "))) // 566
  }
}
