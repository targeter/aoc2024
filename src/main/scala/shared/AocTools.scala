package shared

abstract class AocTools(day: Int) {
  private val inputGetter = new InputGetter(2024)

  def inputLines: Seq[String] = inputGetter.get(day)

  def inputBlocks(separator: String = "\n\n"): Seq[String] = inputBlob.split(separator).toVector

  def inputBlockLines(blockSeparator: String = "\n\n"): Seq[Seq[String]] = inputBlocks(blockSeparator).map(_.split("\n").toVector)

  def inputLineInts(separator: String = ","): Seq[Seq[Int]] = inputLines.map(parseInts(_, separator)).toVector

  def inputInts: Seq[Int] = inputLines.map(_.toInt)

  def inputBlob = inputLines.mkString("\n")

  def inputGrid: Seq[Seq[Char]] = inputLines.toVector.map(_.toVector)

  def parseInts(line: String, sep: String = ","): Seq[Int] = line.split(sep).map(_.toInt).toVector
  def parseIntLines(lines: Seq[String], sep: String = ","): Seq[Seq[Int]] = lines.map(parseInts(_, sep))

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  def lcm(a: Long, b: Long): Long = (a * b) / gcd(a,b)
  def lcm(l: Seq[Long]): Long = l.foldLeft(1L)((a,b) => (a * b) / gcd(a,b))
}

package object ops {

  implicit class SeqOps[T](seq: Seq[T]) {
    def rotate(n: Int): Seq[T] = seq.drop(n) ++ seq.take(n)
    def halves = seq.splitAt(seq.length / 2)
  }

  implicit class StringOps[T](input: String) {
    def splitLines: List[String] = input.split("\n").toList
    def asInts: List[Int] = splitLines.map(_.toInt)

    def parseInts: Seq[Int] = input.trim.split("\\s+").map(_.toInt).toList
    def parseInts(sep: String): Seq[Int] = input.trim.split(sep).map(_.toInt).toList
    def parseLongs: Seq[Long] = input.trim.split("\\s+").map(_.toLong).toList

    def toBinInt: Int = Integer.parseInt(input, 2)
    def toBinLong: Long = BigInt.apply(input, 2).longValue

    def halves: (String, String) = input.splitAt(input.length / 2)

    def halvesSeq: Seq[String] = {
      import shapeless.syntax.std.tuple._
      halves.toList
    }
  }
}
