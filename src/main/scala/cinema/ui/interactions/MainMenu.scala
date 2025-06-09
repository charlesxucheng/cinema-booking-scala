package cinema.ui.interactions

import cats.data.State
import cinema.ui.base.UserInteraction.{Input, Result}
import cinema.ui.base.{LS, UserInteraction}
import cinema.ui.{AppState, CinemaExit}

case object MainMenu extends UserInteraction[AppState] {

  private case class MenuOption(
      description: String,
      result: Result[AppState]
  )

  private val menuOptions: Map[String, MenuOption] = Map(
    "1" -> MenuOption(
      "[1] Set movie & show times",
      Result("", SetMovieAndShowTimes)
    ),
    "2" -> MenuOption("[2] Define seating map", Result("", DefineSeatingMap)),
    "3" -> MenuOption("[3] Book tickets", Result("", BookTickets)),
    "4" -> MenuOption("[4] Exit", Result("", CinemaExit))
  )

  override def getPrompt: String = getPrompt(AppState.empty)

  override def getPrompt(state: AppState): String = {

    val firstOptionDynamicMessage = menuOptions("1").description +
      state.movie
        .map(movie =>
          s" (${movie.title}, ${movie.duration}, ${movie.showTimes.size} show times)"
        )
        .getOrElse("")

    val secondOptionDynamicMessage = menuOptions("2").description +
      state.cinemaHall.map(hall => s" (${hall.capacity} seats)").getOrElse("")

    s"""$firstOptionDynamicMessage
       |$secondOptionDynamicMessage
       |[3] Book tickets
       |[4] Exit
       |Please enter your selection:""".stripMargin
  }
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
