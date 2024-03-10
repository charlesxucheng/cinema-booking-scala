package cinema

import cinema.Row.SeatBlocks
import org.apache.commons.lang3.Range


object Row:
  def apply(id: Int, name: String, seatCount: Int) = new Row(id, name, seatCount, SeatBlocks(Seq(Range.of(1, seatCount))))

  def getMiddlePoint(numberOfSeats: Int): Int = ((numberOfSeats + 1) / 2 + 1)

  type SeatBlock = Range[Integer]

  /**
   * SeatBlocks represents a collection of seats numbers in a particular Row.
   * The ranges are indicated by from and to integers (inclusive).
   * The ranges are positive and cannot exceed a max value (the Row's max number of seats)
  **/
  opaque type SeatBlocks = Seq[SeatBlock]

  object SeatBlocks:
    def apply(ranges: Seq[SeatBlock]): SeatBlocks = {
      require(ranges.forall(range => range.getMaximum > 0 && range.getMinimum > 0)
        && !isOverlapping(ranges)
      )
      ranges
    }

    def fromPairs(ranges: Seq[(Int, Int)]): SeatBlocks = SeatBlocks(ranges.map(pair => Range.of(pair._1, pair._2)))


    private def isOverlapping(ranges: Seq[SeatBlock]) =
      if (ranges.length <= 1) false
      else {
        val sorted = ranges.sortWith(_.getMinimum <= _.getMinimum)

        sorted
          .zip(sorted.tail)
          .exists(pairOfRanges => pairOfRanges._1.getMaximum >= pairOfRanges._2.getMinimum)
      }

  extension (seatBlocks: SeatBlocks)
    def seatCount: Int = seatBlocks.map(range => range.getMaximum - range.getMinimum + 1).sum

case class Row private(id: Int, name: String, seatCount: Int, availableSeats: SeatBlocks):
  require(id > 0 && seatCount > 0 && availableSeats.seatCount <= seatCount)

sealed trait SeatingMap:
  def capacity: Int

  def availableSeatCount: Int

  val seats: Seq[Row]

// The rows and columns are both 1-based, with row 1 nearest to the screen, and column 1 being the leftmost seat.
case class RectangularSeatingMap(rows: Int, cols: Int) extends SeatingMap:

  val seats: Seq[Row] = (1 to rows).map(rowId => Row(rowId, rowId.toString, cols))

  override def capacity: Int = rows * cols

  override def availableSeatCount: Int = capacity
