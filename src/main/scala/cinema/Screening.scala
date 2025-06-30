package cinema

import cinema.SeatAllocationStrategy.AllocatedSeatBlocks

import java.time.LocalTime

object Screening {
  def apply(
      movie: Movie,
      seatingMap: SeatingMap,
      showTimeIndex: Int
  ): Screening =
    Screening(movie, CinemaHall(seatingMap), showTimeIndex)
}

case class Screening(movie: Movie, cinemaHall: CinemaHall, showTimeIndex: Int) {
  require(showTimeIndex >= 0 && showTimeIndex < movie.showTimes.length)

  val showTime: LocalTime = movie.showTimes(showTimeIndex)
  val seatingMap: SeatingMap = cinemaHall.seatingMap
  private val allocationStrategy =
    DefaultSeatAllocationStrategy // Can be injected if required

  def holdSeatsForBooking(
      numberOfSeats: Int
  ): (Seq[AllocatedSeatBlocks], Screening) = {
    val allocationResult =
      allocationStrategy.allocateSeats(seatingMap, numberOfSeats)
    val updatedSeatingMap = allocationResult.updatedSeatingMap
    (
      allocationResult.allocatedSeats,
      Screening(movie, allocationResult.updatedSeatingMap, showTimeIndex)
    )
  }

  def holdSUserSpecifiedSeatsForBooking(
      numOfSeats: Int,
      startingRow: Int,
      startingCol: Int,
      seatsHeldForBooking: Seq[AllocatedSeatBlocks]
  ): (Seq[AllocatedSeatBlocks], Screening) = {
    assert(seatingMap.seatingMapBeforeHold.nonEmpty)

    val allocationResult = allocationStrategy.allocateSeats(
      seatingMap.seatingMapBeforeHold.get,
      numOfSeats,
      startingRow,
      startingCol
    )
    val updatedSeatingMap = allocationResult.updatedSeatingMap
    (
      allocationResult.allocatedSeats,
      Screening(movie, allocationResult.updatedSeatingMap, showTimeIndex)
    )
  }

  def confirmBooking(allocatedSeats: Seq[AllocatedSeatBlocks]): Screening = {
    val updatedSeatingMap = seatingMap.confirmSeatsForBooking(allocatedSeats)
    this.copy(cinemaHall = CinemaHall(updatedSeatingMap))
  }
}
