package cinema

import cinema.Row.{SeatBlocks, isOverlapping}

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.Range

object Row {
  type SeatBlock = Range

  /** SeatBlocks represents a collection of seats numbers in a particular Row.
    * The ranges are indicated by from and to integers (inclusive). The ranges
    * are positive and cannot exceed a max value (the Row's max number of seats)
    */
  opaque type SeatBlocks = Seq[SeatBlock]
  val maxRowId = 702 // A-Z, AA - ZZ, 26 * 26 + 26
  private val minRowId = 1

  def apply(id: Int, seatCount: Int): Row =
    Row.apply(id, rowIdToName(id), seatCount)

  def apply(id: Int, name: String, seatCount: Int) =
    new Row(id, name, seatCount, SeatBlocks.empty, SeatBlocks.empty)

  private def rowIdToName(id: Int): String = {
    require(
      id >= minRowId && id <= maxRowId,
      s"Row ID must be between $minRowId and $maxRowId (both inclusive)"
    )
    decimalToAlpha(id)
  }

  private def decimalToAlpha(num: Int): String = {
    @tailrec
    def decimalToAlphaRec(
        num: Int,
        firstDigit: Option[Char]
    ): (Option[Char], Char) =
      if (num <= 26) (firstDigit, ('A' + num - 1).toChar)
      else
        decimalToAlphaRec(
          num - 26,
          firstDigit.map(c => (c + 1).toChar).orElse(Some('A'))
        )

    val digits = decimalToAlphaRec(num, None)

    s"${digits._1.getOrElse("")}${digits._2}"
  }

  private def isOverlapping(ranges: SeatBlocks): Boolean =
    if (ranges.length <= 1)
      false
    else {
      val sorted = ranges.sortWith(_.start <= _.start)
      sorted
        .zip(sorted.tail)
        .exists(pairOfRanges => pairOfRanges._1.end >= pairOfRanges._2.start)
    }

  extension (seatBlock: SeatBlock)
    def of(from: Int, to: Int): SeatBlock = {
      require(from > 0 && to > 0, "Both from and to must be greater than 0")
      Range.inclusive(from, to)
    }

  object SeatBlocks {
    def apply(ranges: Seq[SeatBlock]): SeatBlocks = {
      require(
        ranges.forall(range => range.end > 0 && range.start > 0),
        s"The start and end of all ranges in $ranges should be greater than 0"
      )
      require(
        !isOverlapping(ranges),
        s"None of the ranges in $ranges should be overlapping"
      )

      ranges.sortWith(_.start <= _.start) // always sort the incoming Ranges
    }

    def empty: SeatBlocks = SeatBlocks(Seq.empty)

    def of(ranges: Seq[(Int, Int)]): SeatBlocks = SeatBlocks(
      ranges.map(pair => Range.inclusive(pair._1, pair._2))
    )

    @tailrec
    def minusRec(
        a: SeatBlocks,
        b: SeatBlocks,
        accumulatedDiff: SeatBlocks
    ): SeatBlocks = a.toList match {
      case Nil =>
        accumulatedDiff
      case x :: xs =>
        b.toList match {
          case Nil =>
            accumulatedDiff ++ a
          case y :: ys =>
            //            assert(x.contains(y))
            val diffToAccumulate =
              if (x.start < y.start) SeatBlocks.of(Seq((x.start, y.start - 1)))
              else SeatBlocks.empty
            val diffToPassOn =
              if (x.end > y.end) SeatBlocks.of(Seq((y.end + 1, x.end)))
              else SeatBlocks.empty

            minusRec(diffToPassOn, ys, accumulatedDiff ++ diffToAccumulate)
        }
    }
  }

  extension (seatBlocks: SeatBlocks)

    def nonEmpty: Boolean = seatBlocks.nonEmpty

    def seatCount: Int =
      seatBlocks.map(range => range.end - range.start + 1).sum

    def map[B](f: Range => B): Seq[B] = seatBlocks.map(f)

    def forall(f: Range => Boolean): Boolean = seatBlocks.forall(f)

    def contains(range: Range): Boolean = seatBlocks.contains(range)

    @targetName("append")
    def :+(seatBlock: SeatBlock): SeatBlocks = SeatBlocks(
      seatBlocks :+ seatBlock
    )

    @targetName("concat")
    def ++(b: SeatBlocks): SeatBlocks = SeatBlocks(seatBlocks ++ b)

    @targetName("diff")
    def --(other: SeatBlocks): SeatBlocks = {
      val diffResult = SeatBlocks.minusRec(seatBlocks, other, Seq.empty)
      assert(seatBlocks.containsEveryElementOf(diffResult))
      assert(diffResult.forall(!other.contains(_)))
      diffResult
    }

    private def containsEveryElementOf(other: SeatBlocks): Boolean =
      other.flatMap(_.toSet).toSet.subsetOf(seatBlocks.flatMap(_.toSet).toSet)

}

case class Row private (
    id: Int,
    name: String,
    seatCount: Int,
    bookedSeats: SeatBlocks,
    bookingInProgressSeats: SeatBlocks
) {
  require(id > 0, s"Row ID must be greater than zero: $id")
  require(name.length <= 20, s"Row name must not exceed 20 characters: $name")
  require(
    seatCount > 0 && bookedSeats.seatCount <= seatCount,
    s"The row must have positive seat count ($seatCount) and the count of booked seats (${bookedSeats.seatCount} must not be larger than the total seat count"
  )

  val midPoint: Float = (seatCount + 1).toFloat / 2

  val availableSeats: SeatBlocks =
    SeatBlocks(
      Seq(Range.inclusive(1, seatCount))
    ) -- bookedSeats -- bookingInProgressSeats

  def holdSeatsForBooking(seatBlocks: SeatBlocks): Row = {
    require(
      !isOverlapping(bookedSeats ++ bookingInProgressSeats ++ seatBlocks),
      s"Seats to be reserved ($seatBlocks) cannot include seats that are already booked ($bookedSeats) or reserved ($bookingInProgressSeats)."
    )
    this.copy(bookingInProgressSeats = bookingInProgressSeats ++ seatBlocks)
  }

  def confirmBooking(seatBlocks: SeatBlocks): Row = {
    require(
      seatBlocks.forall(bookingInProgressSeats.contains),
      s"Cannot find seats to be confirmed ($seatBlocks) in the list of held seats ($bookingInProgressSeats)."
    )
    this.copy(
      bookedSeats = bookedSeats ++ seatBlocks,
      bookingInProgressSeats = bookingInProgressSeats -- seatBlocks
    )
  }
}
