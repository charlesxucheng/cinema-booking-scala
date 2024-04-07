package cinema

import cinema.Row.{SeatBlocks, isOverlapping}
import org.apache.commons.lang3.Range
import scala.collection.immutable.Range as SRange

import scala.annotation.{tailrec, targetName}

object Row {
  type SeatBlock = Range[Integer]
  /**
   * SeatBlocks represents a collection of seats numbers in a particular Row.
   * The ranges are indicated by from and to integers (inclusive).
   * The ranges are positive and cannot exceed a max value (the Row's max number of seats)
   * */
  opaque type SeatBlocks = Seq[SeatBlock]

  val minRowId = 1
  val maxRowId = 52
  def apply(id: Int, seatCount: Int): Row = Row.apply(id, rowIdToName(id), seatCount)

  def apply(id: Int, name: String, seatCount: Int) = new Row(id, name, seatCount, SeatBlocks.empty)

  private def isOverlapping(ranges: Seq[SeatBlock]): Boolean =
    if (ranges.length <= 1)
      false
    else {
      val sorted = ranges.sortWith(_.getMinimum <= _.getMinimum)
      sorted
        .zip(sorted.tail)
        .exists(pairOfRanges => pairOfRanges._1.getMaximum >= pairOfRanges._2.getMinimum)
    }

  private def rowIdToName(id: Int): String = {
    require(id >= minRowId && id <= maxRowId, s"Row ID must be between $minRowId and $maxRowId (both inclusive)")
    if (id <= 26) ('A' + id - 1).toChar.toString
    else "A" + ('A' + id - 27).toChar.toString
  }

  extension (seatBlock: SeatBlock)
    def size: Int = seatBlock.getMaximum - seatBlock.getMinimum + 1
    def of(from: Int, to: Int): SeatBlock = {
      require(from > 0 && to > 0, "Both from and to must be greater than 0")
      Range.of(from, to)
    }
    private def toScalaRange: SRange = SRange.inclusive(seatBlock.getMinimum.toInt, seatBlock.getMaximum.toInt)

  object SeatBlocks {
    def apply(ranges: Seq[SeatBlock]): SeatBlocks = {
      require(ranges.forall(range => range.getMaximum > 0 && range.getMinimum > 0),
        s"The start and end of all ranges in $ranges should be greater than 0")
      require(!isOverlapping(ranges), s"None of the ranges in $ranges should be overlapping")

      ranges.sortWith(_.getMinimum <= _.getMinimum) // always sort the incoming Ranges
    }

    def empty: SeatBlocks = SeatBlocks(Seq.empty)

    def fromPairs(ranges: Seq[(Int, Int)]): SeatBlocks = SeatBlocks(ranges.map(pair => Range.of(pair._1, pair._2)))

    @tailrec
    def minusRec(a: SeatBlocks, b: SeatBlocks, accumulatedDiff: SeatBlocks): SeatBlocks = {
      a.toList match {
        case x :: xs =>
          b.toList match {
            case y :: ys =>
              assert(x.containsRange(y))
              val diffToAccumulate =
                if (x.getMinimum < y.getMinimum) SeatBlocks.fromPairs(Seq((x.getMinimum, y.getMinimum - 1)))
                else SeatBlocks.empty
              val diffToPassOn =
                if (x.getMaximum > y.getMaximum) SeatBlocks.fromPairs(Seq((y.getMaximum + 1, x.getMaximum)))
                else SeatBlocks.empty

              minusRec(diffToPassOn, ys, accumulatedDiff ++ diffToAccumulate)
            case Nil =>
              accumulatedDiff ++ a
          }
        case Nil =>
          accumulatedDiff
      }
    }
  }


  extension (seatBlocks: SeatBlocks)
    def seatCount: Int = seatBlocks.map(range => range.getMaximum - range.getMinimum + 1).sum

    def toSeq: Seq[SeatBlock] = seatBlocks

//    def map[B](f: Range[Integer] => B): Seq[B] = seatBlocks.map(f)

    @targetName("prepended")
    def +:(seatBlock: SeatBlock): SeatBlocks = SeatBlocks(seatBlock +: seatBlocks)

    @targetName("concat")
    def ++(b: SeatBlocks): SeatBlocks = SeatBlocks(seatBlocks ++ b)

    @targetName("diff")
    def --(other: SeatBlocks): SeatBlocks = {
      val diffResult = SeatBlocks.minusRec(seatBlocks, other, Seq.empty)
      assert(seatBlocks.containsEveryElementOf(diffResult))
      assert(diffResult.forall(!other.contains(_)))
      diffResult
    }

    private def containsEveryElementOf(other: SeatBlocks): Boolean = {
      val ranges1Elements = seatBlocks.flatMap(block => block.toScalaRange)
      val ranges2Elements = other.flatMap(block => block.toScalaRange)
      ranges2Elements.forall { e => ranges1Elements.contains(e) }
    }

}

case class Row private(id: Int, name: String, seatCount: Int, bookedSeats: SeatBlocks) {
  require(id > 0, s"Row ID must be greater than zero: $id")
  require(name.length <= 20, s"Row name must not exceed 20 characters: $name")
  require(seatCount > 0 && bookedSeats.seatCount <= seatCount,
    s"The row must have positive seat count ($seatCount) and the count of booked seats (${bookedSeats.seatCount} must not be larger than the total seat count")

  val midPoint: Int = seatCount / 2 + 1

  val availableSeats: SeatBlocks = SeatBlocks(Seq(Range.of(1, seatCount))) -- bookedSeats

  def assignSeats(seatBlocks: SeatBlocks): Row = {
    require(!isOverlapping((bookedSeats ++ seatBlocks).toSeq),
      s"Seats to be booked ($seatBlocks) cannot overlap with seats that are already booked ($bookedSeats).")
    this.copy(bookedSeats = bookedSeats ++ seatBlocks)
  }
}