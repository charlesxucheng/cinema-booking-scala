package cinema

import cinema.RectangularSeatingMap.{maxCols, maxRows}
import cinema.SeatingMap.SeatStatus.*
import cinema.SeatingMapView.{longHeader, shortHeader}

object SeatingMap {

  private def convertToStrings(m: SeatingMap): Seq[String] = {
    m.seatsForDisplay.map(r => {
      val allSeats = r.bookedSeats.map((Booked, _)) ++
        r.bookingInProgressSeats.map((HeldForBooking, _)) ++
        r.availableSeats
          .map((Available, _))
          .sortBy(x => x._2.start)

      val seatSpacing =
        " " * Math.max(m.row(1).seatCount.toString.length - 1, 1)

      val symbols = allSeats
        .sortBy(x => x._2.start)
        .map(b => s"$seatSpacing${b._1.displaySymbol}" * b._2.size)

      val maxIdLength = m.seats.map(_.name.length).max

      symbols.foldLeft(r.name.padTo(maxIdLength, ' '))(_ + _)
    })
  }

  enum SeatStatus(val displaySymbol: String) {
    case Available extends SeatStatus(".")
    case Booked extends SeatStatus("#")
    case HeldForBooking extends SeatStatus("o")
  }

  given SeatingMapView[SeatingMap] with
    extension (m: SeatingMap)
      def viewAsSingleString: String = {
        val content = viewAsMultiPartContent(m)
        val headerLine = content.header + System.lineSeparator()
        val separationLine = content.separator + System.lineSeparator()
        val seatLines =
          content.seats.foldLeft("")((a, b) => a + b + System.lineSeparator())
        headerLine + separationLine + seatLines
      }

    extension (m: SeatingMap)
      def viewAsMultiPartContent: SeatingMapViewContent = {

        val seatsContent = convertToStrings(m)
        val maxWidth = seatsContent.map(_.length).max
        val headerContent =
          if (maxWidth < longHeader.length) shortHeader else longHeader

        val leftPadding = (maxWidth - headerContent.length) / 2
        val rightPadding = maxWidth - headerContent.length - leftPadding
        val header =
          " ".repeat(leftPadding) + headerContent + " ".repeat(rightPadding)

        val separator = "-".repeat(maxWidth)

        SeatingMapViewContent(header, separator, seatsContent)
      }
}

sealed trait SeatingMap {
  val seats: IndexedSeq[Row]
  val seatsForDisplay: IndexedSeq[Row]
  val availableRows: IndexedSeq[Row]

  def capacity: Int

  def row(rowId: Int): Row = {
    require(
      rowId >= 0 && rowId <= seats.length,
      s"Row ID must be between 0 and ${seats.length} (inclusive): $rowId"
    )
    seats(rowId - 1)
  }

  def availableSeatCount: Int

  def bookSeats(updatedSeats: IndexedSeq[Row]): SeatingMap

  protected def updateRows(updatedRows: Seq[Row]): IndexedSeq[Row] =
    updatedRows.foldLeft(seats)((seats, row) => seats.updated(row.id - 1, row))

}
// The rows and columns are both 1-based, with row 1 furthest from the screen, and column 1 being the leftmost seat.
case class RectangularSeatingMap(rows: Int, cols: Int, seats: IndexedSeq[Row])
    extends SeatingMap {
  require(
    rows > 0 && rows <= maxRows,
    s"Number of rows must be greater than zero and less than or equal to $maxRows: $rows"
  )
  require(
    cols > 0 && cols <= maxCols,
    s"Number of columns must be greater than zero and less than or equal to $maxCols: $cols"
  )
  require(
    seats.size == rows && seats.forall(_.seatCount == cols),
    s"Seats provided must have exactly $rows rows and $cols columns."
  )

  override val availableRows: IndexedSeq[Row] =
    seats.filter(row => row.availableSeats.nonEmpty)

  override val seatsForDisplay: IndexedSeq[Row] = seats.reverse

  override def capacity: Int = rows * cols

  override def availableSeatCount: Int =
    seats.map(_.availableSeats.seatCount).sum

  override def bookSeats(bookedSeats: IndexedSeq[Row]): SeatingMap =
    this.copy(seats = updateRows(bookedSeats))
}

object RectangularSeatingMap {
  val maxRows = 100
  val maxCols = 200

  def apply(rows: Int, cols: Int): RectangularSeatingMap =
    RectangularSeatingMap(
      rows,
      cols,
      (1 to rows).map(rowId => Row(rowId, cols))
    )
}
