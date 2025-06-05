package cinema.ui.interactions

import cats.data.State
import cinema.ui.AppState
import cinema.ui.base.UserInteraction

case object BookTickets extends UserInteraction[AppState] {
  override def getPrompt: String = ???
  override def handleInput(
      input: String
  ): State[AppState, UserInteraction.Result[AppState]] = ???
}
