package cinema.ui

import cinema.UnitSpec
import org.scalatest.matchers.should.Matchers.*

class MainMenuTest extends UnitSpec {
  "A MainMenu user interaction" when {
    "\"[1] Set movie & show times\" is selected by user for the first time" should {
      val inputs = Table(
        "input",
        "1",
        " 1",
        "1 ",
        " 1  "
      )
      "go to the SetMovieAndShowTimes user interaction with an empty app state" in {
        val initialAppState = AppState.empty
         forAll(inputs) { input =>
           val result = MainMenu.handleInput(input)
             .run(initialAppState)
             .value
           result._1 shouldBe initialAppState
           result._2.interaction.value shouldBe SetMovieAndShowTimes
         }
      }
    }

    "\"[4] Exit\" is selected by user" should {
      val inputs = Table(
        "input",
        "4",
        " 4",
        "4 ",
        " 4  "
      )
      "exit the application" in {
        val initialAppState = AppState.empty
         forAll(inputs) { input =>
           val result = MainMenu.handleInput(input)
             .run(initialAppState)
             .value
           result._1 shouldBe initialAppState
           result._2.interaction.left.value shouldBe CinemaExit
         }
      }
    }

    "any other input is selected by user" should {
      val inputs = Table(
        "input",
        "2",
        " ",
        "",
        "1230",
        "%&^$",
        "111111111111111111111111111111111111111111111111111",
        LS
      )

      "display an error message" in {
        val initialAppState = AppState.empty
         forAll(inputs) { input =>
           val result = MainMenu.handleInput(input)
             .run(initialAppState)
             .value
           result._2.outputMessage shouldBe MainMenu.invalidInputMessage(input)
         }
      }

      "repeat the same interaction with an empty app state" in {
        val initialAppState = AppState.empty
         forAll(inputs) { input =>
           val result = MainMenu.handleInput(input)
             .run(initialAppState)
             .value
           result._1 shouldBe initialAppState
           result._2.interaction.value shouldBe MainMenu
         }
      }
    }
  }
}
