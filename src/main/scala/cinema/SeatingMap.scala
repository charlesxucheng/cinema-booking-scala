package cinema

import cinema.Row.{SeatBlocks, isOverlapping}
import org.apache.commons.lang3.Range

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable.ListBuffer

sealed trait SeatingMap {
  val seats: IndexedSeq[Row]
  val availableRows: IndexedSeq[Row]

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
  require(seats.size == rows && seats.forall(_.seatCount == cols),
    s"Seats provided must have exactly $rows rows and $cols columns.")

  override val availableRows: IndexedSeq[Row] = seats.filter(row => row.availableSeats.toSeq.nonEmpty).reverse

  override def capacity: Int = rows * cols

  override def availableSeatCount: Int = seats.map(_.availableSeats.seatCount).sum

  override def bookSeats(bookedSeats: IndexedSeq[Row]): SeatingMap = this.copy(seats = updateRows(bookedSeats))
}