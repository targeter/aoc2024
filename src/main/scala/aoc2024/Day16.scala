package aoc2024

import shared._

import scala.collection.mutable

case object Day16 extends AocTools(day = 16) {
  case class Pos(x: Int, y: Int) {
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
    def -(other: Pos): Pos = Pos(x - other.x, y - other.y)
  }

  // Define the directions
  sealed trait Direction {
    def apply(pos: Pos): Pos
  }
  case object North extends Direction { override def apply(pos: Pos): Pos = pos + Pos(0, -1) }
  case object East  extends Direction { override def apply(pos: Pos): Pos = pos + Pos(1, 0) }
  case object South extends Direction { override def apply(pos: Pos): Pos = pos + Pos(0, 1) }
  case object West  extends Direction { override def apply(pos: Pos): Pos = pos + Pos(-1, 0) }

  val directions: Seq[Direction] = Seq(North, East, South, West)

  // Define the state for the priority queue
  case class State(pos:Pos, direction: Direction, cost: Int)

  type Grid = Map[Pos, Int]

  // Define the maze solver
  object MazeSolver {
    val TURN_COST = 1000
    val MOVE_COST = 1

    def solve(maze: Grid, start: Pos, end: Pos): (Int, List[List[Pos]]) = {
      implicit val stateOrdering: Ordering[State] = Ordering.by(-_.cost)
      val pq = mutable.PriorityQueue[State]()
      pq.enqueue(State(start, East, 0))

      val minCost = mutable.Map[(Pos, Direction), Int]()
      val paths = mutable.Map[(Pos, Direction), List[List[Pos]]]()
      paths((start, East)) = List(List(start))


      while (pq.nonEmpty) {
        val state = pq.dequeue()

        if (state.pos == end) {
          val finalPaths = paths.toList.collect {
            case ((`end`, _), p) => p
          }.flatten
          return (state.cost,finalPaths)
        }

        // Explore all possible moves
        for (nextDirection <- directions) {
          val nextPos = nextDirection(state.pos)
          val turnCost = if (nextDirection == state.direction) 0 else TURN_COST
          val newCost = state.cost + MOVE_COST + turnCost

          if (maze.get(nextPos).contains(0)) {
            val stateKey = (nextPos, nextDirection)

            if (!minCost.contains(stateKey) || newCost < minCost(stateKey)) {
              minCost(stateKey) = newCost
              paths(stateKey) = paths(state.pos, state.direction).map(_ :+ nextPos)
              pq.enqueue(State(nextPos, nextDirection, newCost))
            } else if (newCost == minCost(stateKey)) {
              val existingPaths = paths.getOrElse(stateKey, List())
              val newPaths = paths((state.pos, state.direction)).map(_ :+ nextPos)
              paths(stateKey) = existingPaths ++ newPaths
            }
          }
        }
      }

      (-1, Nil)
    }
  }

  def parse(input: Seq[String]) = {
    for {
      (line, y) <- input.zipWithIndex
      (char, x) <- line.zipWithIndex
    } yield Pos(x, y) ->  (if(char == '#') 1 else 0)


  }.toMap


  def step1(lines: Seq[String]): Int = {
    val maze = parse(lines)
    val start = Pos(1, lines.length - 2)
    val end = Pos(lines.head.length - 2, 1)

    MazeSolver.solve(maze, start, end)._1
  }

  def step2(lines: Seq[String]): Int =  {
    val maze = parse(lines)
    val start = Pos(1, lines.length - 2)
    val end = Pos(lines.head.length - 2, 1)


    val (score, paths) = MazeSolver.solve(maze, start, end)
    paths.toSet.flatten.size
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines))
    println("Step 2: " + step2(inputLines))
  }
}

