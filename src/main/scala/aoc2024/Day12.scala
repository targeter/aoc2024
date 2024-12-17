package aoc2024

import shared._

import scala.annotation.tailrec

case object Day12 extends AocTools(day = 12) {

  def parse(lines: Seq[String]): Grid = lines.map(_.toSeq)

  type Grid = Seq[Seq[Char]]

  case class Pos(x: Int, y: Int) {
    lazy val left = Pos(x - 1, y)
    lazy val right = Pos(x + 1, y)
    lazy val up = Pos(x, y - 1)
    lazy val down = Pos(x, y + 1)

    def -(other: Pos): Pos = Pos(x - other.x, y - other.y)
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)

    def around = Seq(this.copy(x = this.x - 1), this.copy(x = this.x + 1), this.copy(y = this.y - 1), this.copy(y = this.y + 1))
  }

  implicit class GridOps(grid: Grid) {
    val positions: Seq[Pos] = for {
      y <- 0 until grid.size
      x <- 0 until grid(y).size
    } yield Pos(x, y)

    def apply(pos: Pos) = grid(pos.y)(pos.x)

    def allWhere(f: Char => Boolean): Seq[Pos] = positions.filter(p => f(apply(p)))

    def get(pos: Pos): Char = grid(pos.y)(pos.x)

    def around(pos: Pos): Seq[Pos] = {
      pos.around
        .filter(p => p.x >= 0 && p.x < grid.head.size && p.y >= 0 && p.y < grid.size && p != pos)
    }

    def getPlot(pos: Pos): Set[Pos] = {
      val target     = this(pos)
      val initVissit = around(pos).filter(p => apply(p) == target)
      getPlot(initVissit.toSet, Set(pos))
    }

    @tailrec
    final def getPlot(toVist: Set[Pos], acc: Set[Pos]): Set[Pos] = {
      if (toVist.isEmpty) acc
      else {
        val pos          = toVist.head
        val target       = this(pos)
        val newPositions = around(pos).filter(p => apply(p) == target).filterNot(acc.contains)
        getPlot(toVist.tail ++ newPositions, acc + pos)
      }
    }

    @tailrec
    final def getPlots(positions: Seq[Pos], accPlots: Seq[Set[Pos]]): Seq[Set[Pos]] = {
      if (positions.isEmpty) accPlots
      else {
        val curr +: rest       = positions
        val plot               = getPlot(curr)
        val remainingPositions = rest.filterNot(plot)
        getPlots(remainingPositions, accPlots :+ plot)
      }
    }

  }

  def circumference(positions: Set[Pos]): Int = {
    positions.toSeq.foldLeft(0) { (acc, pos) =>
      acc + pos.around.count(!positions(_))
    }
  }

  def sides(plot: Set[Pos]) = plot.toSeq.map { pos =>
    val hasNoTopAndLeftNeighbor = !plot.contains(pos.up) && !plot.contains(pos.left)
    val hasNeighborsAroundTopLeft = plot.contains(pos.up) && plot.contains(pos.left) && !plot.contains(pos.up.left)
    val isTopLeft = hasNoTopAndLeftNeighbor || hasNeighborsAroundTopLeft // external or internal corner
    val hasNoTopRightNeighbors = !plot(pos.up) && !plot(pos.right)
    val hasNeighborsAroundTopRight = plot(pos.up) && plot(pos.right) && !plot(pos.up.right)
    val isTopRight = hasNoTopRightNeighbors || hasNeighborsAroundTopRight
    val hasNoBottomLefttNeighbors = !plot(pos.down) && !plot(pos.left)
    val hasNeigborsAroundBottomLeft = plot(pos.down) && plot(pos.left) && !plot(pos.down.left)
    val isBottomLeft = hasNoBottomLefttNeighbors || hasNeigborsAroundBottomLeft
    val hasNoBottomRightNeighbors = !plot.contains(pos.down) && !plot.contains(pos.right)
    val hasNeighborsAroundBottomRight = plot.contains(pos.down) && plot.contains(pos.right) && !plot.contains(pos.down.right)
    val isBottomRight = hasNoBottomRightNeighbors || hasNeighborsAroundBottomRight

    Seq(isTopLeft, isTopRight, isBottomLeft, isBottomRight).count(identity)
  }

  def step1(lines: Seq[String]): Int = {
    val grid     = parse(lines)
    val allPlots = grid.getPlots(grid.positions, Seq.empty).filterNot(_.isEmpty)

    allPlots.map(cca => cca.size * circumference(cca)).sum
  }

  def step2(lines: Seq[String]): Int = {
    val grid     = parse(lines)
    val allPlots = grid.getPlots(grid.positions, Seq.empty).filterNot(_.isEmpty)
    allPlots.map(p => p.size * sides(p).sum).sum
  }

  def main(args: Array[String]): Unit = {
    val ex = """
               |RRRRIICCFF
               |RRRRIICCCF
               |VVRRRCCFFF
               |VVRCCCJFFF
               |VVVVCJJCFE
               |VVIVCCJJEE
               |VVIIICJJEE
               |MIIIIIJJEE
               |MIIISIJEEE
               |MMMISSJEEE""".stripMargin.linesIterator.toSeq.tail

    println("Step ex: " + step1(ex)) // 1930
    println("Step 1: " + step1(inputLines)) // 1522850
    println("Step 2: " + step2(inputLines)) // 953738
  }
}
