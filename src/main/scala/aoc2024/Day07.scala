package aoc2024

import shared._

case object Day07 extends AocTools(day = 7) {

  def parse(lines: Seq[String]): Seq[(Long, Seq[Long])] = lines.map { case s"$a: $b" =>
    a.toLong -> b.split(" ").map(_.toLong).toList
  }

  type Op = (Long, Long) => Long
  val add: Op = _ + _
  val mul: Op = _ * _
  val concat: Op = (a,b) => (a.toString + b.toString).toLong
  val part1Ops: Seq[Op] = Seq(add, mul)
  val part2Ops: Seq[Op] = part1Ops :+ concat


  def isValid(ops: Seq[Op], goal: Long, acc: Long, inputs: Seq[Long]): Boolean = {
    if (inputs.isEmpty || acc > goal) acc == goal
    else {
      ops.exists { op =>
        isValid(ops, goal, op(acc, inputs.head), inputs.tail)
      }
    }
  }

  def doIt(ops: Seq[Op], lines: Seq[(Long, Seq[Long])]): Long = {
    lines.collect {
      case (goal, head :: tail) if isValid(ops, goal, head, tail) => goal
    }.sum
  }

  def step1(lines: Seq[String]): Long = doIt(part1Ops, parse(lines))

  def step2(lines: Seq[String]): Long = doIt(part2Ops, parse(lines))

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines))
    println("Step 2: " + step2(inputLines))
  }
}
