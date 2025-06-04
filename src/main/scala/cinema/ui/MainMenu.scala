package cinema.ui

import cats.data.State
import cinema.ui.base.{LS, UserInteraction}
import cinema.ui.base.UserInteraction.{Input, Result}

case object MainMenu extends UserInteraction[AppState] {
  override def getPrompt: String =
    """[1] Set movie & show times
      |[2] Define seating map
      |[3] Book tickets
      |[4] Exit
      |Please enter your selection:"
      |""".stripMargin

  override def handleInput(
      input: Input
  ): State[AppState, UserInteraction.Result[AppState]] = State { currentState =>
    {
      input.trim match {
        case "1" => (currentState, Result("", SetMovieAndShowTimes))
        case "2" => (currentState, Result("", DefineSeatingMap))
        case "3" => (currentState, Result("", BookTickets))
        case "4" => (currentState, Result("", CinemaExit))
        case _ =>
          (currentState, Result(invalidInputMessage(input), MainMenu))
      }
    }

  }
}
