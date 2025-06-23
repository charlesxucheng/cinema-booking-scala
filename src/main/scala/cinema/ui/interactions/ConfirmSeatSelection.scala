package cinema.ui.interactions

import cats.data.State
import cinema.ui.AppState
import cinema.ui.base.UserInteraction
import cinema.ui.base.UserInteraction.Result

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
    input.trim match {
      case "" =>
        (
          (for {
            showtimeId <- currentState.selectedShowTimeId
            numberOfTickets <- currentState.selectedNumberOfTickets
            screening <- currentState.selectedScreening
            seatsHeldForBooking <- currentState.seatsHeldForBooking
          } yield {
            val updatedScreening = screening.confirmBooking(seatsHeldForBooking)
            currentState.confirmedBooking(showtimeId, updatedScreening)
          }).get,
          Result("", MainMenu)
        )
      case _ => ???

    }
  }

  override def getPrompt: String =
    "Something went wrong. Press Enter to return to main menu."

  private def parseInput(input: String): Result[AppState] = ???
}
