package aoc2024

import shared._

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

case object Day06 extends AocTools(day = 6) {

  case class Position(x: Int, y: Int, looped: Int = 0) {
    def move(dir: Direction) = dir(this)
  }

  sealed trait Direction {
    def apply(pos: Position): Position = this match {
      case North => pos.copy(y = pos.y - 1)
      case South => pos.copy(y = pos.y + 1)
      case East  => pos.copy(x = pos.x + 1)
      case West  => pos.copy(x = pos.x - 1)
    }
  }
  case object North extends Direction
  case object South extends Direction
  case object East  extends Direction
  case object West  extends Direction

  case class Guard(pos: Position, direction: Direction, history: Set[(Position, Direction)] = Set.empty, looped: Int = 0) {

    def turnRight: Guard = direction match {
      case North => copy(direction = East)
      case South => copy(direction = West)
      case East  => copy(direction = South)
      case West  => copy(direction = North)
    }

    def move: Position = pos.move(direction)
  }

  def parse(lines: Seq[String]): (Guard, Map[Position, Char]) = {
    val map = for {
      y <- lines.indices
      x <- lines(y).indices

    } yield Position(x, y) -> lines(y)(x)

    val guard = map.collectFirst {
      case (pos, '^') => Guard(pos, North)
      case (pos, 'v') => Guard(pos, South)
      case (pos, '>') => Guard(pos, East)
      case (pos, '<') => Guard(pos, West)
    }.get
    guard -> map.toMap
  }

  def step1(lines: Seq[String]): Int = {
    val (guard, map) = parse(lines)

    val visited = walk2(map, guard)

    visited.history.map(_._1).toSet.size
  }

  @tailrec
  def walk2(map: Map[Position, Char], guard: Guard): Guard = {
    if (guard.history.contains((guard.pos, guard.direction))) guard.copy(looped = guard.looped + 1)
    else {
      val nextPos = guard.move
      map.get(nextPos) match {
        case Some('#') => walk2(map, guard.turnRight.copy(history = guard.history + ((guard.pos, guard.direction))))
        case Some(_)   => walk2(map, guard.copy(pos = nextPos, history = guard.history + ((guard.pos, guard.direction))))
        case None      => guard.copy(history = guard.history + ((guard.pos, guard.direction)))
      }
    }
  }

  def step2(lines: Seq[String]): Int = {
    val (guard, map) = parse(lines)

    val history       = walk2(map, guard).history.map(_._1).toSet - guard.pos
    val mapCandidates = history.map(pos => map.updated(pos, '#'))
    mapCandidates.par.map(walk2(_, guard)).count(_.looped > 0)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines)) // 4665
    println("Step 2: " + step2(inputLines)) // 1688
  }
}
