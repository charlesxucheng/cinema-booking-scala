package cinema.ui

import cinema.{RectangularSeatingMap, UnitSpec}
import org.scalatest.matchers.should.Matchers.*

class DefineSeatingMapTest extends UnitSpec {
  "A DefineSeatingMap user interaction" when {
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
        ("input", "Rows", "Cols"),
        ("5 5", 5, 5),
        (" 1 1 ", 1, 1),
        ("8 10 ", 8, 10),
        (" 10 8", 10, 8),
        (s"${RectangularSeatingMap.maxRows} ${RectangularSeatingMap.maxCols}", RectangularSeatingMap.maxRows, RectangularSeatingMap.maxCols)
      )

      "capture the movie info and return to main menu" in {
        forAll(validInputs) {
          (input, expectedRows, expectedCols) =>
            val initialState = AppState.empty
            val (newState, result) = DefineSeatingMap
              .handleInput(input)
              .run(initialState)
              .value

            val seatingMap = newState.theatre.get.seatingPlan
            seatingMap shouldBe RectangularSeatingMap(expectedRows, expectedCols)
            result.outputMessage should include regex """A \d+\-seat seating map has been defined."""
            result.interaction.value shouldBe MainMenu
        }
      }
    }

    "invalid input is provided" should {
      val invalidInputs = Table(
        "input",
        "TestInput",
        "8 10 12",
        "0 1",
        "-10 10",
        "800000000000000000000000000000000000 101",
        "ABC DEF"
      )

      "return an error message and repeat the same interaction" in {
        forAll(invalidInputs) { input =>
          val initialState = AppState.empty
          val (newState, result) = DefineSeatingMap
            .handleInput(input)
            .run(initialState)
            .value

          newState shouldBe initialState
          result.outputMessage should include ("Invalid input format. Please use:")
          result.interaction.value shouldBe DefineSeatingMap
        }
      }
    }
  }

}
