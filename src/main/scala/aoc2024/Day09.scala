package aoc2024

import shared._

import scala.annotation.tailrec

case object Day09 extends AocTools(day = 9) {
  sealed trait DiskBlock {
    def size: Int
  }
  case class FreeBlock(size: Int)          extends DiskBlock {
    def fit(fileBlock: FileBlock): (Vector[DiskBlock] => Vector[DiskBlock], DiskBlock) = {
      if (size == fileBlock.size) (identity, fileBlock)
      else if (size > fileBlock.size) ((blocks) => FreeBlock(size - fileBlock.size) +: blocks, fileBlock)
      else
        (
          (blocks) => blocks :+ FileBlock(id = fileBlock.id, size = fileBlock.size - this.size),
          FileBlock(id = fileBlock.id, size = this.size)
        )
    }

    def fit2(fileBlock: FileBlock): Vector[DiskBlock] = {
      if (fileBlock.size == size) Vector(fileBlock)
      else Vector(fileBlock, FreeBlock(size - fileBlock.size))
    }
  }
  case class FileBlock(id: Int, size: Int) extends DiskBlock

  def parse(diskMap: Seq[Int]): Vector[DiskBlock] = {
    val evenSize      = if (diskMap.size % 2 == 0) diskMap.size else diskMap.size + 1
    val (files, free) = diskMap
      .padTo(evenSize, 0)
      .sliding(2, 2)
      .map { case Seq(a, b) => (a, b) }
      .toVector
      .unzip

    val fileBlocks = files.zipWithIndex.map { case (f, i) => FileBlock(i, f) }
    val freeBlocks = free.map(FreeBlock(_))
    fileBlocks
      .zip(freeBlocks)
      .flatMap(x => Vector(x._1, x._2))
  }

  @tailrec
  def defrag(diskMap: Vector[DiskBlock], newDiskMap: Vector[DiskBlock] = Vector.empty): Vector[DiskBlock] = {
    diskMap match {
      case dm if dm.isEmpty                               => newDiskMap
      case (file: FileBlock) +: rest                      => defrag(rest, newDiskMap :+ file)
      case (free: FreeBlock) +: rest :+ (_: FreeBlock)    => defrag(free +: rest, newDiskMap)
      case (free: FreeBlock) +: rest :+ (file: FileBlock) =>
        val (remainder, placed) = free.fit(file)
        defrag(remainder(rest), newDiskMap :+ placed)
      case (free: FreeBlock) +: rest                      => defrag(rest, newDiskMap :+ free) // free block at the end
    }
  }

  @tailrec
  def defrag2(diskMap: Vector[DiskBlock], newDiskMap: Vector[DiskBlock] = Vector.empty): Vector[DiskBlock] = {
    if (diskMap.isEmpty) newDiskMap
    else {
      val (rest :+ last) = diskMap

      diskMap.last match {
        case FreeBlock(size) => defrag2(rest, FreeBlock(size) +: newDiskMap)
        case file: FileBlock =>
          val indexOfFirstFit = rest.indexWhere {
            case FreeBlock(size) => size >= file.size
            case _               => false
          }

          if (indexOfFirstFit == -1) defrag2(rest, file +: newDiskMap)
          else {
            val before = rest.slice(0, indexOfFirstFit)
            val free   = rest(indexOfFirstFit).asInstanceOf[FreeBlock]
            val after  = rest.slice(indexOfFirstFit + 1, rest.size)
            defrag2(before ++ free.fit2(file) ++ after, FreeBlock(file.size) +: newDiskMap)
          }

      }
    }

  }

  def checksum(filesystem: Vector[DiskBlock]): Long = {
    filesystem
      .flatMap {
        case FileBlock(id, size) => Vector.fill(size)(id.toLong)
        case FreeBlock(size)     => Vector.fill(size)(0L)
      }
      .zipWithIndex
      .map { case (id, i) => id * i.toLong }
      .sum
  }

  def step1(line: String): Long = {
    val diskMap = parse(line.map(_.asDigit))
    val blocks  = defrag(diskMap)
    checksum(blocks)
  }

  def step2(line: String): Long = checksum(defrag2(parse(line.map(_.asDigit))))

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines.head)) // 6200294120911
    println("Step 2: " + step2(inputLines.head)) // 6227018762750
  }
}
