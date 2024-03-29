package cinema

import cinema.Row.{SeatBlocks, isOverlapping}
import org.apache.commons.lang3.Range

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable.ListBuffer


object Row {
  type SeatBlock = Range[Integer]
  /**
   * SeatBlocks represents a collection of seats numbers in a particular Row.
   * The ranges are indicated by from and to integers (inclusive).
   * The ranges are positive and cannot exceed a max value (the Row's max number of seats)
   * */
  opaque type SeatBlocks = Seq[SeatBlock]

  def apply(id: Int, seatCount: Int): Row = Row.apply(id, id.toString, seatCount)

  private def isOverlapping(ranges: Seq[SeatBlock]): Boolean =
    if (ranges.length <= 1)
      false
    else {
      val sorted = ranges.sortWith(_.getMinimum <= _.getMinimum)
      sorted
        .zip(sorted.tail)
        .exists(pairOfRanges => pairOfRanges._1.getMaximum >= pairOfRanges._2.getMinimum)
    }

  extension (seatBlock: SeatBlock)
    def size: Int = seatBlock.getMaximum - seatBlock.getMinimum + 1
    def of(from: Int, to: Int): SeatBlock = {
      require(from >= 0)
      Range.of(from, to)
    }

  def apply(id: Int, name: String, seatCount: Int) = new Row(id, name, seatCount, SeatBlocks.empty)

  object SeatBlocks {
    def apply(ranges: Seq[SeatBlock]): SeatBlocks = {
      require(ranges.forall(range => range.getMaximum > 0 && range.getMinimum > 0)
        && !isOverlapping(ranges)
      )
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

    def map[B](f: Range[Integer] => B): Seq[B] = seatBlocks.map(f)

    @targetName("prepended")
    def +:(seatBlock: SeatBlock): SeatBlocks = SeatBlocks(seatBlock +: seatBlocks)

    @targetName("concat")
    def ++(b: SeatBlocks): SeatBlocks = SeatBlocks(seatBlocks ++ b)

    @targetName("minus")
    def --(b: SeatBlocks): SeatBlocks = SeatBlocks.minusRec(seatBlocks, b, Seq.empty)

}

case class Row private(id: Int, name: String, seatCount: Int, bookedSeats: SeatBlocks) {
  require(id > 0 && seatCount > 0 && bookedSeats.seatCount <= seatCount)
  val midPoint: Int = seatCount / 2 + 1

  val availableSeats: SeatBlocks = SeatBlocks(Seq(Range.of(1, seatCount))) -- bookedSeats

  def assignSeats(seatBlocks: SeatBlocks): Row = this.copy(bookedSeats = bookedSeats ++ seatBlocks)
}

sealed trait SeatingMap {
  val seats: IndexedSeq[Row]

  def capacity: Int

  def availableSeatCount: Int

  def bookSeats(updatedSeats: IndexedSeq[Row]): SeatingMap

  protected def updateRows(updatedRows: Seq[Row]): IndexedSeq[Row] =
    updatedRows.foldLeft(seats)((seats, row) => seats.updated(row.id - 1, row))

}

object RectangularSeatingMap {
  def apply(rows: Int, cols: Int): RectangularSeatingMap =
    RectangularSeatingMap(rows, cols, (1 to rows).map(rowId => Row(rowId, rowId.toString, cols)))
}

// The rows and columns are both 1-based, with row 1 nearest to the screen, and column 1 being the leftmost seat.
case class RectangularSeatingMap(rows: Int, cols: Int, seats: IndexedSeq[Row]) extends SeatingMap {
  require(seats.size == rows && seats.forall(_.seatCount == cols))

  override def capacity: Int = rows * cols

  override def availableSeatCount: Int = seats.map(_.availableSeats.seatCount).sum

  override def bookSeats(updatedSeats: IndexedSeq[Row]): SeatingMap = this.copy(seats = updateRows(updatedSeats))
}