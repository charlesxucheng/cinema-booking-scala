package cinema

import cinema.Row.SeatBlocks.SeatBlocks
import org.apache.commons.lang3.Range


object Row:
  def apply(id: Int, name: String, seatCount: Int) = new Row(id, name, seatCount, SeatBlocks(Seq(Range.of(1, seatCount))))

  // SeatBlocks represents a collection of seats numbers in a particular Row.
  // The ranges are indicated by from and to integers (inclusive).
  // The ranges are positive and cannot exceed a max value (the Row's max number of seats)
  object SeatBlocks:
    opaque type SeatBlocks = Seq[Range[Integer]]

    def apply(ranges: Seq[Range[Integer]]): SeatBlocks = {
      require(ranges.forall(range => range.getMaximum > 0 && range.getMinimum > 0))
      ranges
    }

    extension (seatBlock: SeatBlocks)
      def seatCount: Int = seatBlock.map(range => range.getMaximum - range.getMinimum + 1).sum

case class Row private(id: Int, name: String, seatCount: Int, availableSeats: SeatBlocks):
  require(id >= 0 && seatCount > 0 && availableSeats.seatCount <= seatCount)

sealed trait SeatingMap:
  def capacity: Int

  def availableSeatCount: Int

case class RectangularSeatingMap(rows: Int, cols: Int) extends SeatingMap:

  val seats: Seq[Row] = Seq.empty

  override def capacity: Int = rows * cols

  override def availableSeatCount: Int = capacity
