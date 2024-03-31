package cinema

import cinema.Row.{SeatBlocks, isOverlapping}
import org.apache.commons.lang3.Range

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable.ListBuffer

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

  override def bookSeats(updatedSeats: IndexedSeq[Row]): SeatingMap = {
    this.seats zip updatedSeats
    this.copy(seats = updateRows(updatedSeats))
  }
}