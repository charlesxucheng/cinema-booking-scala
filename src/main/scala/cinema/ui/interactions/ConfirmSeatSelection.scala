package cinema.ui.interactions

import cats.data.State
import cinema.ui.AppState
import cinema.ui.base.UserInteraction

case object ConfirmSeatSelection extends UserInteraction[AppState] {
  override def getPrompt(state: AppState): String = {
    val firstLine = 
      s"You chose ${state.selectedNumberOfTickets.get} tickets for the ${state.screenings(state.selectedShowTimeId.get).showTime} showtime."
    val screenDisplay = state.screenings(state.selectedShowTimeId.get).cinemaHall.seatingMap.viewAsSingleString  
    s"""$firstLine
       |$screenDisplay
       |Confirm seat selection by pressing Enter, or press Enter to return to main menu:
       |""".stripMargin
  }

  override def handleInput(
      input: String
  ): State[AppState, UserInteraction.Result[AppState]] = State { currentState =>
    ???
  }

  override def getPrompt: String = "Something went wrong. Press Enter to return to main menu."
}
