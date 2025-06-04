package cinema.ui

import cats.data.State
import cinema.ui.base.UserInteraction

case object BookTickets extends UserInteraction[AppState] {
  override def getPrompt: String = ???
  override def handleInput(
      input: String
  ): State[AppState, UserInteraction.Result[AppState]] = ???
}
