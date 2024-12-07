package aoc2024

import shared._

case object Day03 extends AocTools(day = 3) {

  def step1(input: String): Long = {
    val mulRegex = """mul\((\d+),(\d+)\)""".r
    mulRegex
      .findAllIn(input)
      .map { case s"mul($a,$b)" => a.toLong * b.toLong }
      .sum
  }

  def step2(lines: String): Long = {
    val allRegex = s"""(mul\\(\\d+,\\d+\\))|(do\\(\\))|(don't\\(\\))""".r

    allRegex
      .findAllIn(inputBlob)
      .foldLeft((true, 0L)) {
        case ((_, sum), "do()")              => (true, sum)
        case ((_, sum), "don't()")           => (false, sum)
        case ((enabled, sum), s"mul($a,$b)") => if (enabled) (enabled, sum + (a.toLong * b.toLong)) else (enabled, sum)
      }
      ._2
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputBlob)) // 160672468
    println("Step 2: " + step2(inputBlob)) // 84893551
  }
}
