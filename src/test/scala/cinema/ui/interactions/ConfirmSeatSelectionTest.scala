package cinema.ui.interactions

import cinema.*
import cinema.TestFixtures.TWO_HOURS
import cinema.ui.AppState
import org.scalatest.matchers.should.Matchers.*

import java.time.LocalTime

class ConfirmSeatSelectionTest extends UnitSpec {
  val numberOfSeats = 4
  val showtimeIndex = 0
  private val testMovie = Movie("TestMovie", TWO_HOURS, LocalTime.of(14, 30))
  private val seatingMap = DefaultSeatAllocationStrategy
    .allocateSeats(
      RectangularSeatingMap(10, 10),
      numberOfSeats
    )
    .updatedSeatingMap

  private val initialstate = AppState.empty
    .setMovie(testMovie)
    .setCinemaHall(CinemaHall(seatingMap))
    .setShowTimeAndNumberOfTickets(showtimeIndex, numberOfSeats)

  private val allocationResult =
    initialstate.screenings(showtimeIndex).holdSeatsForBooking(numberOfSeats)

  private val stateWithSeatsHeld = initialstate.setHeldSeats(
    showtimeIndex,
    allocationResult._2,
    allocationResult._1
  )

  "ConfirmSeatSelection user interaction" should {
    "display the show time and number of tickets selected" in {
      val prompt = ConfirmSeatSelection.getPrompt(initialstate)
      prompt should include(
        s"You chose ${initialstate.selectedNumberOfTickets.get} tickets for the ${initialstate.selectedShowTime.get} showtime."
      )
    }

    "display the seating map showing selected seats" in {
      val prompt = ConfirmSeatSelection.getPrompt(initialstate)
      prompt should include(". . . o o o o . . .")
    }
  }

  it when {
    "user hit enter without other input" should {
      "confirm the booking and return to main menu" in {
        val result = ConfirmSeatSelection
          .handleInput("")
          .run(stateWithSeatsHeld)
          .value
        result._1.getSeatingMapForSelectedShowTime
          .map(_.availableSeatCount)
          .get shouldBe 96
        result._2.interaction.value shouldBe MainMenu
      }
    }
  }

}
