package cinema.ui.interactions

import cats.data.State
import cinema.ui.AppState
import cinema.ui.base.UserInteraction.Result
import cinema.ui.base.{LS, UserInteraction}
import cinema.{CinemaHall, DefaultSeatAllocationStrategy}

case object SelectShowTimeAndNumberOfSeats extends UserInteraction[AppState] {
  override def getPrompt(state: AppState): String = {
    if (state.movie.isEmpty || state.cinemaHall.isEmpty)
      "No movie show times or seating map has been defined."
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
    """.stripMargin
    }
  }

  override def handleInput(
      input: String
  ): State[AppState, UserInteraction.Result[AppState]] = State { currentState =>
    {
      if (currentState.movie.isEmpty || currentState.cinemaHall.isEmpty) {
        (
          currentState,
          Result("Please set movie show times and seating map first.", MainMenu)
        )
      } else if (input.trim == "") {
        (currentState, Result("", MainMenu))
      } else {
        parseInput(input) match {
          case Right((showtimeId, numberOfTickets)) =>
            val movie = currentState.movie.get
            val showtime = currentState.screenings.get(showtimeId - 1)
            showtime
              .map { screening =>
                {
                  val seatingMap = screening.cinemaHall.seatingPlan
                  try {
                    val updatedSeatingMap = DefaultSeatAllocationStrategy
                      .allocateSeats(seatingMap, numberOfTickets)
                      ._2
                    (
                      currentState
                        .updateScreening(
                          showtimeId,
                          screening
                            .copy(cinemaHall = CinemaHall(updatedSeatingMap))
                        )
                        .setShowTimeAndNumberOfTickets(
                          showtimeId,
                          numberOfTickets
                        ),
                      Result("", ConfirmSeatSelection)
                    )
                  } catch {
                    case e: IllegalArgumentException =>
                      (
                        currentState,
                        Result(
                          s"""Invalid input format. ${e.getMessage}
                           |""".stripMargin,
                          this
                        )
                      )
                  }
                }
              }
              .getOrElse(
                (currentState, Result(s"Invalid input format. Invalid show time Id $showtimeId selected.", this))
              )
          case Left(errorMessage) =>
            (currentState, Result(s"Invalid input format. $errorMessage", this))
        }
      }
    }
  }

  override def getPrompt: String = getPrompt(AppState.empty)

  private def parseInput(input: String): Either[String, (Int, Int)] = {
    val splitInput = input.trim.split(" ").map(_.trim)
    if (splitInput.length != 2)
      Left(
        "Expect two numbers but found ${splitInput.length}."
      )
    else
      try {
        Right((splitInput(0).toInt, splitInput(1).toInt))
      } catch {
        case e: IllegalArgumentException => Left(e.getMessage)
      }
  }
}
