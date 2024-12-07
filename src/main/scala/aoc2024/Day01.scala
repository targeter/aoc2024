package aoc2024

import shared._

case object Day01 extends AocTools(day = 1) {
  def parse(lines: Seq[String]): (Seq[Int], Seq[Int]) = {
    lines.map {
      case s"$a   $b" => a.toInt -> b.toInt
    }.unzip
  }

  def step1(lines: Seq[String]): Int = {
    val (listA, listB) = parse(lines)
    listA.sorted.zip(listB.sorted).map { case (a, b) => (a - b).abs }.sum
  }

  def step2(lines: Seq[String]): Int =  {
    val (listA, listB) = parse(lines)
    listA.map(i => listB.count(_ == i) * i).sum
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines)) //1530215
    println("Step 2: " + step2(inputLines)) //26800609
  }
}

