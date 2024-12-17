package aoc2024

import shared._

import scala.annotation.tailrec

case object Day15 extends AocTools(day = 15) {
  sealed trait MapObject
  trait BoxPart extends MapObject {
    def otherPartAt: Move
    def otherPart: BoxPart
  }
  object MapObject {
    def apply(c: Char): MapObject = c match {
      case '#' => Wall
      case '.' => Empty
      case 'O' => Box
      case '[' => LeftBox
      case ']' => RightBox
      case '@' => Robot
    }
  }
  case object Wall extends MapObject
  case object Empty    extends MapObject
  case object Box      extends MapObject
  case object LeftBox  extends BoxPart {
    lazy val otherPart: BoxPart = RightBox
    val otherPartAt: Move       = MoveRight
  }
  case object RightBox extends BoxPart {
    lazy val otherPart: BoxPart = LeftBox
    val otherPartAt: Move       = MoveLeft
  }

  case object Robot extends MapObject

  case class Pos(x: Int, y: Int) {
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
  }
  sealed trait Move              {
    def apply(pos: Pos): Pos
  }

  sealed trait HorizontalMove extends Move
  sealed trait VerticalMove   extends Move

  object Move {
    def apply(char: Char): Move = char match {
      case '^' => MoveUp
      case 'v' => MoveDown
      case '<' => MoveLeft
      case '>' => MoveRight
    }
  }
  case object MoveUp extends VerticalMove {
    def apply(pos: Pos): Pos = pos.copy(y = pos.y - 1)
  }
  case object MoveDown extends VerticalMove {
    def apply(pos: Pos): Pos = pos.copy(y = pos.y + 1)
  }
  case object MoveLeft extends HorizontalMove {
    def apply(pos: Pos): Pos = pos.copy(x = pos.x - 1)
  }
  case object MoveRight extends HorizontalMove {
    def apply(pos: Pos): Pos = pos.copy(x = pos.x + 1)
  }

  type Grid = Map[Pos, MapObject]
  implicit class GridOps(grid: Grid) {
    def robot: Pos = grid.collectFirst { case (pos, Robot) => pos }.get

    @tailrec
    final def firstNonBoxPos(curr: Pos, move: Move): Pos = {
      if (grid(curr) != Box) curr
      else firstNonBoxPos(move(curr), move)
    }

    def printit() = {
      grid.groupBy(_._1.y).toSeq.sortBy(_._1).foreach { case (y, row) =>
        row.toSeq.sortBy(_._1.x).foreach { case (pos, obj) =>
          val char = obj match {
            case Wall     => '#'
            case Empty    => '.'
            case Box      => 'O'
            case LeftBox  => '['
            case RightBox => ']'
            case Robot    => '@'
          }
          print("" + char)
        }
        println()
      }
    }
  }

  def parse(input: String, scale: Boolean = false): (Grid, Seq[Move]) = {
    val Seq(gridInput, instructInput) = input.split("\n\n").toSeq
    val scaled                        = if (scale) embiggen(gridInput.linesIterator.toSeq) else gridInput.linesIterator.toSeq
    val grid: Map[Pos, MapObject]     = (for {
      (line, y) <- scaled.zipWithIndex
      (obj, x)  <- line.zipWithIndex
    } yield (Pos(x, y), MapObject(obj))).toMap

    val moves = instructInput.linesIterator.flatMap(_.map(Move(_)).toSeq).toSeq
    (grid, moves)
  }

  def move(grid: Grid, move: Move): Grid = {
    val robot     = grid.robot
    val targetPos = grid.firstNonBoxPos(move(robot), move)
    if (grid(targetPos) == Empty) {
      grid
        .updated(robot, Empty)
        .updated(targetPos, Box)
        .updated(move(robot), Robot)
    } else grid
  }

  def calculateGps(grid: Grid): Int = {
    grid.collect {
      case (Pos(x, y), Box)     => 100 * y + x
      case (Pos(x, y), LeftBox) => 100 * y + x
    }.sum
  }

  def step1(input: String): Int = {
    val (grid, moves) = parse(input)
    val processed     = moves.foldLeft(grid)(move)
    processed.printit()
    calculateGps(processed)

  }

  def move2(grid: Grid, move: Move): Grid = {

    def moveIt(grid: Grid, pos: Pos): Option[Grid] = {
      (grid(pos), move) match {
        case (Empty, _)                            => Some(grid)
        case (Wall, _)                             => None
        case (boxPart: BoxPart, _: VerticalMove)   =>
          moveIt(grid, move(pos)).flatMap { grid1 =>
            moveIt(grid1, boxPart.otherPartAt(move(pos))).map { grid2 =>
              grid2
                .updated(pos, Empty)
                .updated(boxPart.otherPartAt(pos), Empty)
                .updated(move(pos), boxPart)
                .updated(boxPart.otherPartAt(move(pos)), boxPart.otherPart)
            }
          }
        case (boxPart: BoxPart, _: HorizontalMove) =>
          moveIt(grid, move(pos)).map {
            _.updated(move(pos), boxPart)
              .updated(pos, Empty)
          }
      }
    }

    val newRobotPos = move(grid.robot)
    moveIt(grid, newRobotPos)
      .map(_.updated(newRobotPos, Robot).updated(grid.robot, Empty))
      .getOrElse(grid)

  }

  def embiggen(input: Seq[String]) = input.map(_.flatMap {
    case '#' => "##"
    case '.' => ".."
    case 'O' => "[]"
    case '@' => "@."
  })

  def step2(input: String): Int = {
    val (grid, moves) = parse(input, scale = true)
    val processed     = moves.foldLeft(grid)(move2)
    calculateGps(processed)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputBlob)) // 1406628
    println("Step 2: " + step2(inputBlob)) // 1432781
  }
}
