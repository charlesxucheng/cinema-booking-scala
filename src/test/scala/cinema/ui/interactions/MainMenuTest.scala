package cinema.ui.interactions

import cinema.MovieDurations.MovieDuration
import cinema.TestFixtures.TWO_HOURS
import cinema.ui.interactions.{DefineSeatingMap, MainMenu, SelectShowTimeAndNumberOfSeats, SetMovieAndShowTimes}
import cinema.ui.{AppState, CinemaExit}
import cinema.{CinemaHall, Movie, RectangularSeatingMap, UnitSpec}
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
          val result = MainMenu
            .handleInput(input)
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
        val initialAppState = AppState.empty
          .setMovie(
            Movie("TestMovie", TWO_HOURS, LocalTime.of(14, 30))
          )
        forAll(inputs) { input =>
          val result = MainMenu
            .handleInput(input)
            .run(initialAppState)
            .value
          result._1 shouldBe initialAppState
          result._2.interaction.value shouldBe DefineSeatingMap
        }
      }
    }

    "\"[3] Book tickets\" is selected by user" should {
      val inputs = Table(
        "input",
        "3",
        " 3",
        "3 ",
        " 3  "
      )
      "go to the BookTickets user interaction" in {
        val initialAppState = AppState.empty
          .setMovie(
            Movie("TestMovie", TWO_HOURS, LocalTime.of(14, 30))
          )
          .setCinemaHall(CinemaHall(RectangularSeatingMap(10, 10)))
        forAll(inputs) { input =>
          val result = MainMenu
            .handleInput(input)
            .run(initialAppState)
            .value
          result._1 shouldBe initialAppState
          result._2.interaction.value shouldBe SelectShowTimeAndNumberOfSeats
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
          val result = MainMenu
            .handleInput(input)
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
          val result = MainMenu
            .handleInput(input)
            .run(initialAppState)
            .value
          result._2.outputMessage shouldBe MainMenu.invalidInputMessage(input)
        }
      }

      "repeat the same interaction with an empty app state" in {
        val initialAppState = AppState.empty
        forAll(inputs) { input =>
          val result = MainMenu
            .handleInput(input)
            .run(initialAppState)
            .value
          result._1 shouldBe initialAppState
          result._2.interaction.value shouldBe MainMenu
        }
      }
    }

    "movie info has been set" should {
      "display the movie info in the menu option of the prompt" in {
        val state = AppState.empty
          .setMovie(
            Movie.create(
              "TestMovie",
              TWO_HOURS,
              Seq(LocalTime.of(14, 30), LocalTime.of(18, 50))
            )
          )
        MainMenu.getPrompt(state) should include(
          "(TestMovie, 120 minutes, 2 show times)"
        )
      }
    }

    "movie theatre info has been set" should {
      "display the movie theatre info in the menu option of the prompt" in {
        val state = AppState.empty
          .setCinemaHall(CinemaHall(RectangularSeatingMap(10, 10)))
        MainMenu.getPrompt(state) should include("(100 seats)")
      }
    }
  }
}
