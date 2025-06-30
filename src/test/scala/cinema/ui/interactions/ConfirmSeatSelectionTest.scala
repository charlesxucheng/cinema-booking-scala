package cinema.ui.interactions

import cinema.*
import cinema.Row.SeatBlocks
import cinema.SeatAllocationStrategy.AllocatedSeatBlocks
import cinema.TestFixtures.TWO_HOURS
import cinema.ui.AppState
import org.scalatest.matchers.should.Matchers.*

import java.time.LocalTime

class ConfirmSeatSelectionTest extends UnitSpec {
  val numberOfSeats = 4
  val showtimeIndex = 0
  private val testMovie = Movie("TestMovie", TWO_HOURS, LocalTime.of(14, 30))

  private val initialState = AppState.empty
    .setMovie(testMovie)
    .setCinemaHall(CinemaHall(RectangularSeatingMap(10, 10)))
    .setShowTimeAndNumberOfTickets(showtimeIndex, numberOfSeats)

  private val allocationResult =
    initialState.screenings(showtimeIndex).holdSeatsForBooking(numberOfSeats)

  private val stateWithSeatsHeld = initialState.setHeldSeats(
    showtimeIndex,
    allocationResult._2,
    allocationResult._1
  )

  "ConfirmSeatSelection user interaction" should {
    "display the show time and number of tickets selected" in {
      val prompt = ConfirmSeatSelection.getPrompt(stateWithSeatsHeld)
      prompt should include(
        s"You chose ${initialState.selectedNumberOfTickets.get} tickets for the ${initialState.selectedShowTime.get} showtime."
      )
    }

    "display the seating map showing selected seats" in {
      val prompt = ConfirmSeatSelection.getPrompt(stateWithSeatsHeld)
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

  "user enter a seat number to change seat selection" should {
    "change the selected seat and remain in the same user interaction" in {
      val result = ConfirmSeatSelection
        .handleInput("B2")
        .run(stateWithSeatsHeld)
        .value
      result._1.seatsHeldForBooking.get.head shouldBe AllocatedSeatBlocks(
        2,
        SeatBlocks.of(Seq((2, 5)))
      )
      result._2.interaction.value shouldBe ConfirmSeatSelection
    }
  }

  "user hit enter without other input after changing seat selection explicitly" should {
    "confirm the booking with the seats selected and return to main menu" in {
      val step1Result = ConfirmSeatSelection
        .handleInput("B2")
        .run(stateWithSeatsHeld)
        .value

      val step2Result = step1Result._2.interaction.value
        .handleInput("")
        .run(step1Result._1)
        .value

      val resultSeatingMap = step2Result._1.getSeatingMapForSelectedShowTime
      resultSeatingMap.map(_.availableSeatCount).get shouldBe 96
      resultSeatingMap.map(
        _.row(1).availableSeats shouldBe SeatBlocks.of(Seq((1, 10)))
      )

      step2Result._2.interaction.value shouldBe MainMenu
    }
  }

  "user enter an invalid input" should {
    "display error message and remain in the same user interaction" in {
      val result = ConfirmSeatSelection
        .handleInput("invalid input")
        .run(stateWithSeatsHeld)
        .value
      result._1 shouldBe stateWithSeatsHeld
      result._2.interaction.value shouldBe ConfirmSeatSelection
    }
  }

  "user's change starting seat request cannot be fulfilled" should {
    "display error message and remain in the same user interaction" in {
      val result = ConfirmSeatSelection
        .handleInput("B99")
        .run(stateWithSeatsHeld)
        .value
      result._1 shouldBe stateWithSeatsHeld
      result._2.interaction.value shouldBe ConfirmSeatSelection
    }
  }
}
