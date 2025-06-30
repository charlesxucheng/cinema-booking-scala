package cinema.ui

import cats.effect.unsafe.implicits.global
import cinema.UnitSpec
import cinema.ui.interactions.MainMenu
import org.scalatest.matchers.should.Matchers.shouldBe

class HappyPathSmokeTest extends UnitSpec {
  private val mainMenuPrompt =
    """[1] Set movie & show times
      |[2] Define seating map
      |[3] Book tickets
      |[4] Exit
      |Please enter your selection:""".stripMargin

  private val mainMenuPromptWithMovieInfo =
    """[1] Set movie & show times (The Matrix, 136 minutes, 3 show times)
      |[2] Define seating map
      |[3] Book tickets
      |[4] Exit
      |Please enter your selection:""".stripMargin

  private val mainMenuPromptWithMovieInfoAndSeatingMap =
    """[1] Set movie & show times (The Matrix, 136 minutes, 3 show times)
      |[2] Define seating map (200 seats)
      |[3] Book tickets
      |[4] Exit
      |Please enter your selection:""".stripMargin

  private val setMovieAndShowTimesPrompt =
    "Set movie in [Title] [Duration] [Showtime.1(hh:mm)] ... [Showtime.n] format, or press Enter to return to main menu:"

  private val setMovieConfirmationOutput =
    """Movie (The Matrix, 136 minutes, show times: 13:30, 17:00, 20:00) has been set.
      |""".stripMargin

  private val defineSeatingMapPrompt =
    "Enter seating map in [Rows] [Cols] format, or press Enter to return to main menu:"

  private val defineSeatingMapConfirmationOutput =
    """A 200-seat seating map has been defined.
      |""".stripMargin

  private val bookTicketPrompt = """Book tickets for The Matrix
      |Select show time:
      |[1] 13:30
      |[2] 17:00
      |[3] 20:00
      |Enter selected showtime and number of tickets in [showtime id] [# of tickets] format, or press Enter to return to main menu:
      |""".stripMargin

  private val screenLine = "               S C R E E N               "

  private val bookTicketConfirmationOutput =
    "You chose 5 tickets for the 20:00 showtime." + LS + screenLine + LS +
      """-----------------------------------------
      |J . . . . . . . . . . . . . . . . . . . .
      |I . . . . . . . . . . . . . . . . . . . .
      |H . . . . . . . . . . . . . . . . . . . .
      |G . . . . . . . . . . . . . . . . . . . .
      |F . . . . . . . . . . . . . . . . . . . .
      |E . . . . . . . . . . . . . . . . . . . .
      |D . . . . . . . . . . . . . . . . . . . .
      |C . . . . . . . . . . . . . . . . . . . .
      |B . . . . . . . . . . . . . . . . . . . .
      |A . . . . . . . . o o o o o . . . . . . .
      |
      |Confirm seat selection by pressing Enter, or enter the starting seat number (e.g. B2) to change your seat selection:
      |""".stripMargin

  private val secondBookTicketConfirmationOutput =
    "You chose 6 tickets for the 20:00 showtime." + LS + screenLine + LS +
      """-----------------------------------------
      |J . . . . . . . . . . . . . . . . . . . .
      |I . . . . . . . . . . . . . . . . . . . .
      |H . . . . . . . . . . . . . . . . . . . .
      |G . . . . . . . . . . . . . . . . . . . .
      |F . . . . . . . . . . . . . . . . . . . .
      |E . . . . . . . . . . . . . . . . . . . .
      |D . . . . . . . . . . . . . . . . . . . .
      |C . . . . . . . . . . . . . . . . . . . .
      |B . . . . . . . . . . . . . . . . . . . .
      |A . . o o o o o o # # # # # . . . . . . .
      |
      |Confirm seat selection by pressing Enter, or enter the starting seat number (e.g. B2) to change your seat selection:
      |""".stripMargin

  private val secondBookTicketSeatSelectionOutput = {
    "You chose 6 tickets for the 20:00 showtime." + LS + screenLine + LS +
      """-----------------------------------------
      |J . . . . . . . . . . . . . . . . . . . .
      |I . . . . . . . . . . . . . . . . . . . .
      |H . . . . . . . . . . . . . . . . . . . .
      |G . . . . . . . . . . . . . . . . . . . .
      |F . . . . . . . . . . . . . . . . . . . .
      |E . . . . . . . . . . . . . . . . . . . .
      |D . . . . . . . . . . . . . . . . . . . .
      |C . . . . . . . . . . . . . . . . . . . .
      |B . . . . o o o o o o . . . . . . . . . .
      |A . . . . . . . . # # # # # . . . . . . .
      |
      |Confirm seat selection by pressing Enter, or enter the starting seat number (e.g. B2) to change your seat selection:
      |""".stripMargin
  }

  val exitMessage = "Thank you for using the Cinema Booking System!"

  "A cinema booking application" should {
    "run a happy path scenario successfully" in {
      val inputs = List(
        "1",
        "The Matrix 136 13:30 17:00 20:00",
        "2",
        "10 20",
        "3",
        "3 5",
        "",
        "3",
        "3 6",
        "B5",
        "",
        "4"
      )
      val console = MockConsole(inputs)
      val ui = CinemaConsoleUI(console)
      ui.start(MainMenu).unsafeRunSync()

      val expectedOutput: List[String] = List(
        "Welcome to the Cinema Booking System!",
        mainMenuPrompt,
        setMovieAndShowTimesPrompt,
        setMovieConfirmationOutput,
        mainMenuPromptWithMovieInfo,
        defineSeatingMapPrompt,
        defineSeatingMapConfirmationOutput,
        mainMenuPromptWithMovieInfoAndSeatingMap,
        bookTicketPrompt,
        bookTicketConfirmationOutput,
        mainMenuPromptWithMovieInfoAndSeatingMap,
        bookTicketPrompt,
        secondBookTicketConfirmationOutput,
        secondBookTicketSeatSelectionOutput,
        mainMenuPromptWithMovieInfoAndSeatingMap,
        exitMessage
      ).map(_ + LS)

      console.getOutputLines shouldBe expectedOutput
    }
  }
}
