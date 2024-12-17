package aoc2024

import shared._

case object Day13 extends AocTools(day = 13) {
  case class Pos(x: Long, y: Long)                                     {
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y)
  }
  case class Machine(buttonA: Pos, buttonB: Pos, prize: Pos, pos: Pos) {
    def partTwo = copy(prize = prize + Pos(10000000000000L, 10000000000000L))

    def solve(isPartTwo: Boolean = true): Long = {
      val b1 = prize.x * buttonA.y - prize.y * buttonA.x
      val b2 = buttonB.x * buttonA.y - buttonA.x * buttonB.y

      if ((b1 % b2) != 0L) return 0;

      val b  = b1 / b2
      val a1 = prize.x - b * buttonB.x

      if ((a1 % buttonA.x) != 0L) return 0;
      val a = a1 / buttonA.x

      3 * a.toLong + b.toLong
    }
  }

  def parse(input: String): Seq[Machine] = {
    input
      .split("\n\n")
      .map { case s"Button A: X+$ax, Y+$ay\nButton B: X+$bx, Y+$by\nPrize: X=$px, Y=$py" =>
        Machine(Pos(ax.toLong, ay.toLong), Pos(bx.toLong, by.toLong), Pos(px.toLong, py.toLong), Pos(0, 0))
      }
      .toList

  }

  def step1(blob: String): Long = {
    parse(blob).map(_.solve()).sum
  }

  def step2(blob: String): Long = parse(blob).map(_.partTwo.solve(isPartTwo = true)).sum

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputBlob)) // 29877
    println("Step 2: " + step2(inputBlob))
  }
}
