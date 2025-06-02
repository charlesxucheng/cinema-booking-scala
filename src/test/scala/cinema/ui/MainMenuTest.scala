package cinema.ui

import cinema.{Movie, UnitSpec}
import org.scalatest.matchers.should.Matchers.*

import java.time.LocalTime
import scala.concurrent.duration.DurationInt

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

    "\"[2] Define seating map\" is selected by user" should {
      val inputs = Table(
        "input",
        "2",
        " 2",
        "2 ",
        " 2  "
      )

      "go to the DefineSeatingMap user interaction" in {
        val initialAppState = AppState(Some(Movie("TestMovie", 120.minutes, LocalTime.of(14, 30))), None)
        forAll(inputs) { input =>
          val result = MainMenu.handleInput(input)
            .run(initialAppState)
            .value
          result._1 shouldBe initialAppState
          result._2.interaction.value shouldBe DefineSeatingMap
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
        "99",
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
