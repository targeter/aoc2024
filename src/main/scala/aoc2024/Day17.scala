package aoc2024

import shared._

import scala.annotation.tailrec

case object Day17 extends AocTools(day = 17) {

  case class State(pointer: Int, a: Long, b: Long, c: Long, program: Seq[Int], output: Seq[Long]) {

    def next(): State = {
      program(pointer) match {


        case 0 =>
          val litOp = program(pointer + 1)
          val operand     = combo(litOp)
          val result: Long = (a / Math.pow(2, operand)).toLong
          copy(pointer = pointer + 2, a = result)
        case 1 =>
          val operand = program(pointer + 1)
          val result  = b ^ operand
          copy(pointer = pointer + 2, b = result)
        case 2 =>
          val result  = combo(program(pointer + 1)) % 8
          copy(pointer = pointer + 2, b = result)
        case 3 =>
          copy(pointer = if (a != 0) program(pointer + 1) else pointer + 2)
        case 4 =>
          copy(pointer = pointer + 2, b = b ^ c)
        case 5 =>
          val result  = (combo(program(pointer + 1)) & 0b111)
          copy(pointer = pointer + 2, output = output :+ result)
        case 6 =>
          val operand     = combo(program(pointer + 1))
          val denominator = Math.pow(2, operand)
          val result      = (a / denominator).toLong
          copy(pointer = pointer + 2, b = result)
        case 7 =>
          val operand     = combo(program(pointer + 1))
          val denominator = Math.pow(2, operand)
          val result      = (a / denominator).toLong
          copy(pointer = pointer + 2, c = result)

      }
    }

    def combo(operand: Int) = operand match {
      case 4          => a
      case 5          => b
      case 6          => c
      case _  => operand
    }

    @tailrec
    final def run(): State = {
      val nxt = next()
      if(nxt.pointer >= program.length) nxt
      else nxt.run()
    }

  }

  def parse(input: String): State = {
    val s"""Register A: $a\nRegister B: $b\nRegister C: $c\n\nProgram:$instructions""" = input
    State(0, a.toInt, b.toInt, c.toInt, instructions.split(",").map(_.trim.toInt).toSeq, Seq.empty)
  }

  def step1(lines: String): String = {
    val init = parse(lines)
    val end = init.run()
    end.output.mkString(",")
  }
  def step2(lines: String): String = {
    val init = parse(lines)

    def findMatchingDigits(state: State, rightIndex: Int, acc: Long): Option[Long] = {
      (0 to 7).view.map(i => (acc << 3) + i).flatMap { newValue =>
        state.copy(a = newValue).run().output match {
          case state.program => Some(newValue)
          case output if output == state.program.takeRight(rightIndex) => findMatchingDigits(state, rightIndex + 1, newValue)
          case _ => None
        }
      }.headOption
    }

    findMatchingDigits(init, 1, 0L).get.toString


  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputBlob))
    println("Step 2: " + step2(inputBlob))
  }
}
