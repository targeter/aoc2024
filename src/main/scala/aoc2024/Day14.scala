package aoc2024

import org.fusesource.jansi.AnsiConsole
import shared._

import scala.annotation.tailrec

case object Day14 extends AocTools(day = 14) {
  case class Pos(x: Int, y: Int)            {
    def +(other: Pos): Pos                 = Pos(x + other.x, y + other.y)
    def wrap(width: Int, height: Int): Pos = Pos((x + width) % width, (y + height) % height)

    def around = Set(this.copy(x = this.x - 1), this.copy(x = this.x + 1), this.copy(y = this.y - 1), this.copy(y = this.y + 1))
  }
  case class Robot(pos: Pos, velocity: Pos) {
    def next(width: Int, height: Int): Robot = copy(pos = (pos + velocity).wrap(width, height))
  }

  def parse(lines: Seq[String]) = {
    lines.map { case s"p=$px,$py v=$vx,$vy" =>
      Robot(Pos(px.toInt, py.toInt), Pos(vx.toInt, vy.toInt))
    }
  }

  def largestContiguousRegion(positions: Set[Pos]): Int = {
    def floodFill(pos: Pos, visited: Set[Pos] = Set.empty): (Set[Pos], Set[Pos]) = {

      @tailrec
      def helper(toVisit: Set[Pos], region: Set[Pos], visited: Set[Pos]): (Set[Pos], Set[Pos]) = {
        if (toVisit.isEmpty) (region, visited)
        else {
          val current     = toVisit.head
          val restToVisit = toVisit.tail

          if (visited.contains(current)) helper(restToVisit, region, visited)
          else {
            val updatedRegion    = region + current
            val updatedVisited   = visited + current
            val neighborsToVisit = current.around.intersect(positions) -- updatedVisited
            helper(restToVisit ++ neighborsToVisit, updatedRegion, updatedVisited)
          }
        }
      }
      helper(Set(pos), Set.empty[Pos], visited)
    }

    @tailrec
    def helper2(toVisit: Set[Pos], visited: Set[Pos], regions: Seq[Set[Pos]]): Seq[Set[Pos]] = {
      if (toVisit.isEmpty || regions.exists(_.size >= 30)) regions
      else {
        val (reg, vis) = floodFill(toVisit.head, visited)
        helper2(toVisit -- reg, vis ++ reg, regions :+ reg)
      }

    }

    helper2(positions, Set.empty, Seq.empty).map(_.size).max
  }

  def step1(lines: Seq[String], width: Int, height: Int, seconds: Int): Int = {
    val robots   = parse(lines)
    val lazybots = robots.map(robot => LazyList.iterate(robot)(_.next(width, height)))
    val doneBots = lazybots.map(_(seconds))

    val posCount = doneBots.groupMapReduce(_.pos)(_ => 1)(_ + _)

    val yLineIdx = (height / 2)
    val xLineIdx = (width / 2)

    posCount.toSeq
      .groupBy {
        case (Pos(x, y), count) if x < xLineIdx && y < yLineIdx => 0
        case (Pos(x, y), count) if x > xLineIdx && y < yLineIdx => 1
        case (Pos(x, y), count) if x < xLineIdx && y > yLineIdx => 2
        case (Pos(x, y), count) if x > xLineIdx && y > yLineIdx => 3
        case _                                                  => 4
      }
      .removed(4)
      .toSeq
      .map { case (quad, botCount) =>
        botCount.map(_._2).sum
      }
      .product

  }

  def render(robots: Seq[Robot], width: Int, height: Int): String = {
    AnsiConsole.systemInstall()
    val grid = Array.fill(height, width)('.')
    robots.foreach { robot =>
      grid(robot.pos.y)(robot.pos.x) = '#'
    }
    grid.map(_.mkString).mkString("\n")
  }

  def posCount(robots: Seq[Robot]): Map[Pos, Int] = robots.groupBy(_.pos).view.mapValues(_.size).toMap

  def step2(lines: Seq[String], width: Int, height: Int): Int = {
    val robots   = parse(lines)
    val lazybots = robots.map(robot => LazyList.iterate(robot)(_.next(width, height)))

    val iterator = LazyList.iterate(robots)(_.map(_.next(width, height)))
    val hit      = iterator.indexWhere(robots => posCount(robots).values.forall(_ == 1) && largestContiguousRegion(robots.map(_.pos).toSet) > 30)

//    println(render(iterator(hit), width, height))

    hit
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines, 101, 103, 100))
    println("Step 2: " + step2(inputLines, 101, 103))
  }
}
