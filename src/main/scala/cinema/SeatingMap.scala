package cinema

import cinema.Row.{SeatBlocks, isOverlapping}
import cinema.SeatingMapView.{longHeader, shortHeader}
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

  given SeatingMapView[RectangularSeatingMap] with
    extension (m: RectangularSeatingMap) def viewAsSingleString: String = {
        val content = viewAsMultiPartContent(m)
        val headerLine = content.header + System.lineSeparator()
        val separationLine = content.separator + System.lineSeparator()
        val seatLines = content.seats.foldLeft("")((a, b) => a + b + System.lineSeparator())
        headerLine + separationLine + seatLines
      }

    extension (m: RectangularSeatingMap) def viewAsMultiPartContent: SeatingMapViewContent = {

      val seatsContent = convertToStringValues(m)
      val maxWidth = seatsContent.map(_.length).max
      val headerContent = if (maxWidth < longHeader.length) shortHeader else longHeader

      val leftPadding = (maxWidth - headerContent.length) / 2
      val rightPadding = maxWidth - headerContent.length - leftPadding
      val header = " ".repeat(leftPadding) + headerContent + " ".repeat(rightPadding)

      val separator = "-".repeat(maxWidth)

      SeatingMapViewContent(header, separator, seatsContent)
    }

  private def convertToStringValues(m: RectangularSeatingMap): Seq[String] = {
    m.seats.map(r => {
      (1 to r.seatCount).foldLeft(r.id.toString)((a, b) => a + ' ' + 'o')
    })
  }
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

object SeatingMapView {
  val longHeader = "S C R E E N"
  val shortHeader = "S"
}

case class SeatingMapViewContent(header: String, separator: String, seats: Seq[String])

trait SeatingMapView[A <: SeatingMap] {
  extension (a: A) def viewAsSingleString: String
  extension (a: A) def viewAsMultiPartContent: SeatingMapViewContent
}

