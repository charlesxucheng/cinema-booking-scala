package cinema.ui

import cats.data.State
import cinema.ui.base.UserInteraction
import cinema.ui.base.UserInteraction.{Input, Result}

case object MainMenu extends UserInteraction[AppState] {
  override def getPrompt: String =
    """[1] Set movie & show times
      |[2] Display seating map
      |[3] Book tickets
      |[4] Exit
      |""".stripMargin

  override def handleInput(input: Input): State[AppState, UserInteraction.Result[AppState]] = State {
    currentState => {
      input.trim match {
        case "1" => (currentState, Result("", SetMovieAndShowTimes))
        case _ => (currentState, Result(invalidInputMessage(input), MainMenu))
      }
    }

  }
}
