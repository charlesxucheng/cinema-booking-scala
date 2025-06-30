package cinema.ui.interactions

import cats.data.State
import cinema.ui.AppState
import cinema.ui.base.UserInteraction.Result
import cinema.ui.base.{LS, UserInteraction}

case object SelectShowTimeAndNumberOfSeats extends UserInteraction[AppState] {
  override def handleInput(
      input: String
  ): State[AppState, UserInteraction.Result[AppState]] = State { currentState =>
    {
      if (currentState.movie.isEmpty || currentState.cinemaHall.isEmpty) {
        (
          currentState,
          Result("", MainMenu)
        )
      } else if (input.trim == "") {
        (currentState, Result("", MainMenu))
      } else {
        parseInput(input) match {
          case Right((showtimeId, numberOfTickets)) =>
            val showtimeIndex =
              showtimeId - 1 // Convert 1-based display ID to 0-based internal indexing

            try {
              val result = currentState
                .screenings(showtimeIndex)
                .holdSeatsForBooking(numberOfTickets)

              (
                currentState
                  .setShowTimeAndNumberOfTickets(
                    showtimeIndex,
                    numberOfTickets
                  )
                  .setHeldSeats(
                    showtimeIndex,
                    result._2,
                    result._1
                  ),
                Result("", ConfirmSeatSelection)
              )
            } catch {
              case e: IllegalArgumentException =>
                (
                  currentState,
                  Result(
                    s"""Invalid input. ${e.getMessage}
                           |""".stripMargin,
                    this
                  )
                )
              case e: NoSuchElementException =>
                (
                  currentState,
                  Result(
                    s"Invalid input. Showtime [$showtimeId] does not exist.",
                    this
                  )
                )
            }
          case Left(errorMessage) =>
            (currentState, Result(s"Invalid input. $errorMessage", this))
        }
      }
    }
  }

  private def parseInput(input: String): Either[String, (Int, Int)] = {
    val splitInput = input.trim.split(" ").map(_.trim)
    if (splitInput.length != 2)
      Left(
        s"Expected two numbers but found ${splitInput.length}."
      )
    else
      try {
        Right((splitInput(0).toInt, splitInput(1).toInt))
      } catch {
        case e: IllegalArgumentException => Left(e.getMessage)
      }
  }

  override def getPrompt: String = getPrompt(AppState.empty)

  override def getPrompt(state: AppState): String = {
    if (state.movie.isEmpty || state.cinemaHall.isEmpty)
      """No movie show times or seating map has been defined. Please set movie show times and seating map first.
        |Press Enter to return to main menu.""".stripMargin
    else {
      val movie = state.movie.get
      val showTimesList = movie.showTimes.zipWithIndex
        .map { case (time, index) =>
          s"[${index + 1}] $time"
        }
        .mkString(LS)

      s"""Book tickets for ${movie.title}
         |Select show time:
         |$showTimesList
         |Enter selected showtime and number of tickets in [showtime id] [# of tickets] format, or press Enter to return to main menu:
         |""".stripMargin
    }
  }
}
