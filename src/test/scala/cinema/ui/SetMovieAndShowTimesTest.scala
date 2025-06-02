package cinema.ui

import cinema.UnitSpec
import org.scalatest.matchers.should.Matchers.*

import java.time.LocalTime
import scala.concurrent.duration.DurationInt

class SetMovieAndShowTimesTest extends UnitSpec {
  "A SetMovieAndShowTimes user interaction" when {
    "only return key is pressed" should {
      "return to main menu with app state unchanged" in {
        val initialState = AppState.empty
        val (newState, result) = SetMovieAndShowTimes
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
        ("input", "expectedTitle", "expectedDuration", "expectedShowTimes"),
        ("TestMovie 120 12:30", "TestMovie", 120, Seq(LocalTime.of(12, 30))),
        (
          "Test Movie 2 90 10:00 14:30",
          "Test Movie 2",
          90,
          Seq(LocalTime.of(10, 0), LocalTime.of(14, 30))
        ),
        (
          "Test Movie 150 150 15:45 19:00 22:15",
          "Test Movie 150",
          150,
          Seq(LocalTime.of(15, 45), LocalTime.of(19, 0), LocalTime.of(22, 15))
        ),
        (" Test Movie  with   Spaces    120  12:30  ",
          "Test Movie with Spaces",
          120,
          Seq(LocalTime.of(12, 30)))
      )

      "capture the movie info and return to main menu" in {
        forAll(validInputs) {
          (input, expectedTitle, expectedDuration, expectedTimes) =>
            val initialState = AppState.empty
            val (newState, result) = SetMovieAndShowTimes
              .handleInput(input)
              .run(initialState)
              .value

            val movie = newState.movie.get
            movie should have (
              Symbol("title") (expectedTitle),
              Symbol("duration") (expectedDuration.minutes),
              Symbol("showTimes") (expectedTimes)
            )
            result.outputMessage should include regex """Movie \(.+\) has been set."""
            result.interaction.value shouldBe MainMenu
        }
      }
    }

    "invalid input is provided" should {
      val invalidInputs = Table(
        "input",
        "TestMovie", // No duration or times
        "Test Movie 120", // No times
        "120 12:30", // No title
        "TestMovie abc 12:30", // Invalid duration
        "TestMovie 120 25:00", // Invalid show time
        "TestMovie 120 12:60", // Invalid show time
        "TestMovie 120 12:30 13:00 25:00",
        "Test Movie 155 12:30 13:00 18:00"
      )

      "return an error message and repeat the same interaction" in {
        forAll(invalidInputs) { input =>
          val initialState = AppState.empty
          val (newState, result) = SetMovieAndShowTimes
            .handleInput(input)
            .run(initialState)
            .value

          newState shouldBe initialState
          result.outputMessage should include ("Invalid input format. Please use:")
          result.interaction.value shouldBe SetMovieAndShowTimes
        }
      }
    }
  }

}
