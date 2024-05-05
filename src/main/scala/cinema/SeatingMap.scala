package cinema

import cinema.Row.size
import cinema.SeatingMapView.{longHeader, shortHeader}

object SeatingMap {
  private def convertToStringValues(m: SeatingMap): Seq[String] = {
    m.seats.map(r => {
      val allSeats = r.bookedSeats.map((true, _)) ++ r.availableSeats.map((false, _))
        .sortBy(x => x._2.start)
      val symbols = allSeats
        .sortBy(x => x._2.start)
        .map(b => if (b._1) " x" * b._2.size else " o" * b._2.size)

      symbols.foldLeft(r.id.toString)(_ + _)
    })
  }

  given SeatingMapView[SeatingMap] with
    extension (m: SeatingMap) def viewAsSingleString: String = {
      val content = viewAsMultiPartContent(m)
      val headerLine = content.header + System.lineSeparator()
      val separationLine = content.separator + System.lineSeparator()
      val seatLines = content.seats.foldLeft("")((a, b) => a + b + System.lineSeparator())
      headerLine + separationLine + seatLines
    }

    extension (m: SeatingMap) def viewAsMultiPartContent: SeatingMapViewContent = {

      val seatsContent = convertToStringValues(m)
      val maxWidth = seatsContent.map(_.length).max
      val headerContent = if (maxWidth < longHeader.length) shortHeader else longHeader

      val leftPadding = (maxWidth - headerContent.length) / 2
      val rightPadding = maxWidth - headerContent.length - leftPadding
      val header = " ".repeat(leftPadding) + headerContent + " ".repeat(rightPadding)

      val separator = "-".repeat(maxWidth)

      SeatingMapViewContent(header, separator, seatsContent)
    }
}

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

  override val availableRows: IndexedSeq[Row] = seats.filter(row => row.availableSeats.nonEmpty).reverse

  override def capacity: Int = rows * cols

  override def availableSeatCount: Int = seats.map(_.availableSeats.seatCount).sum

  override def bookSeats(bookedSeats: IndexedSeq[Row]): SeatingMap = this.copy(seats = updateRows(bookedSeats))
}
