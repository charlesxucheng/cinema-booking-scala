package cinema.ui.interactions

import cats.data.State
import cinema.{Screening, SeatingMap}
import cinema.ui.AppState
import cinema.ui.base.UserInteraction
import cinema.ui.base.UserInteraction.Result

import scala.util.matching.Regex

case object ConfirmSeatSelection extends UserInteraction[AppState] {
  override def getPrompt(state: AppState): String = {
    val dynamicPrompt = for {
      selectedNumberOfTickets <- state.selectedNumberOfTickets
      selectedShowTimeSeatingMap <- state.getSeatingMapForSelectedShowTime
      selectedShowTime <- state.selectedShowTime
      cinemaHall <- state.cinemaHall
    } yield {
      val firstLine =
        s"You chose $selectedNumberOfTickets tickets for the $selectedShowTime showtime."
      val screenDisplay = selectedShowTimeSeatingMap.viewAsSingleString
      s"""$firstLine
         |$screenDisplay
         |Confirm seat selection by pressing Enter, or enter the starting seat number (e.g. B2) to change your seat selection:
         |""".stripMargin
    }

    dynamicPrompt.getOrElse(
      "Something went wrong. Press Enter to return to main menu."
    )
  }

  override def handleInput(
      input: String
  ): State[AppState, UserInteraction.Result[AppState]] = State { currentState =>
    (for {
      showtimeId <- currentState.selectedShowTimeId
      numberOfTickets <- currentState.selectedNumberOfTickets
      screening <- currentState.selectedScreening
      seatsHeldForBooking <- currentState.seatsHeldForBooking
    } yield {
      if (input.trim == "") {
        val updatedScreening = screening.confirmBooking(seatsHeldForBooking)
        (
          currentState.confirmedBooking(showtimeId, updatedScreening),
          Result("", MainMenu)
        )
      } else
        try {
          parseInput(input, screening) match {
            case Right((rowId, colId)) =>
              val updatedScreening =
                screening.holdSUserSpecifiedSeatsForBooking(
                  numberOfTickets,
                  rowId,
                  colId,
                  seatsHeldForBooking
                )
              (
                currentState.setHeldSeats(
                  showtimeId,
                  updatedScreening._2,
                  updatedScreening._1
                ),
                Result("", ConfirmSeatSelection)
              )
            case Left(errorMessage) =>
              (currentState, Result(s"Invalid input. $errorMessage", this))
          }
        } catch {
          case e: IllegalArgumentException =>
            (currentState, Result(s"Invalid input. ${e.getMessage}", this))
        }
    }).getOrElse(currentState, Result("", MainMenu))

  }

  override def getPrompt: String =
    "Something went wrong. Press Enter to return to main menu."

  private def parseInput(
      input: String,
      screening: Screening
  ): Either[String, (Int, Int)] = {
    val pattern: Regex = """([A-Za-z]+)([0-9]+)""".r
    val startingPosition = pattern.findFirstMatchIn(input).map(_.subgroups)
    val seatingMap = screening.seatingMap

    startingPosition
      .map { p =>
        seatingMap
          .getRowIdByName(p.head)
          .map { rowId =>
            {
              Right(rowId, p(1).toInt)
            }
          }
          .getOrElse(Left(s"Row ${p.head} does not exist."))
      }
      .getOrElse(Left(s"Invalid input $input"))
  }

}
