package cinema.ui.interactions

import cinema.MovieDurations.MovieDuration
import cinema.ui.AppState
import cinema.{CinemaHall, Movie, RectangularSeatingMap, UnitSpec}
import org.scalatest.matchers.should.Matchers.*

import java.time.LocalTime
import scala.concurrent.duration.DurationInt
class SelectShowTimeAndNumberOfSeatsTest extends UnitSpec {

  private val testState = AppState.empty
    .setMovie(
      Movie.create(
        "Avengers",
        MovieDuration(120.minutes),
        Seq(LocalTime.of(10, 0), LocalTime.of(14, 0))
      )
    )
    .setCinemaHall(CinemaHall(RectangularSeatingMap(10, 10)))

  "BookTickets user interaction" when {
    "only return key is pressed" should {
      "return to main menu with app state unchanged" in {
        val initialState = AppState.empty
          .setMovie(
            Movie("Avengers", MovieDuration(120.minutes), LocalTime.of(10, 0))
          )
          .setCinemaHall(CinemaHall(RectangularSeatingMap(10, 10)))
        
        val (newState, result) = SelectShowTimeAndNumberOfSeats
          .handleInput("")
          .run(initialState)
          .value

        newState shouldBe initialState
        result.outputMessage shouldBe ""
        result.interaction.value shouldBe MainMenu
      }
    }

    "valid input is provided" should {
      val validInputs = Table(
        ("input", "Showtime Id", "Number of tickets"),
        ("1 2", 1, 2),
        (" 1 15  ", 1, 15),
        ("2 1 ", 2, 1),
        (" 2 8", 2, 8)
      )

      "save the input into AppState" in {
        val initialState = testState
        forAll(validInputs) { (input, showtimeId, numberOfTickets) =>
          {
            val (newState, result) = SelectShowTimeAndNumberOfSeats
              .handleInput(input)
              .run(initialState)
              .value
            newState.selectedShowTimeId shouldBe Some(showtimeId - 1)
            newState.selectedNumberOfTickets shouldBe Some(numberOfTickets)
          }
        }
      }

      "go to ConfirmSeatSelection user interaction" in {
        val initialState = testState
        forAll(validInputs) { (input, expectedRows, expectedCols) =>
          {
            val (newState, result) = SelectShowTimeAndNumberOfSeats
              .handleInput(input)
              .run(initialState)
              .value
            result.outputMessage shouldBe ""
            result.interaction.value shouldBe ConfirmSeatSelection
          }
        }
      }
    }

    "invalid input is provided" should {
      val invalidInputs = Table(
        "input",
        "1",
        "1 2 3",
        " 1 1500 ",
        "dfsa#%",
        "-1 2",
        "30 4",
        "0,8"
      )

      val initialState = testState

      "display error message" in {
        forAll(invalidInputs) { input =>
          val (newState, result) = SelectShowTimeAndNumberOfSeats
            .handleInput(input)
            .run(initialState)
            .value
          result.outputMessage should include("Invalid input.")
        }
      }
      "remain in the same user interaction" in {
        forAll(invalidInputs) { input =>
          val (newState, result) = SelectShowTimeAndNumberOfSeats
            .handleInput(input)
            .run(initialState)
            .value
          result.interaction.value shouldBe SelectShowTimeAndNumberOfSeats
        }
      }
    }
  }
}
