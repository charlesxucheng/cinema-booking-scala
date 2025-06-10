package cinema.ui.interactions

import cats.data.State
import cinema.ui.AppState
import cinema.ui.base.UserInteraction

case object ConfirmSeatSelection extends UserInteraction[AppState] {
  override def getPrompt: String =
    "Confirm seat selection by pressing Enter, or press Enter to return to main menu:"

  override def handleInput(
      input: String
  ): State[AppState, UserInteraction.Result[AppState]] = State { currentState =>
    ???
  }
}
