package cinema.ui

import cats.data.State
import cinema.ui.base.{LS, UserInteraction}
import cinema.ui.base.UserInteraction.{Input, Result}
import cinema.{MovieTheatre, RectangularSeatingMap}

case object DefineSeatingMap extends UserInteraction[AppState] {
  override def getPrompt: String =
    "Enter seating map in [Rows] [Cols] format, or press Enter to return to main menu:"

  override def handleInput(
      input: Input
  ): State[AppState, UserInteraction.Result[AppState]] = State { currentState =>
    input.trim match {
      case "" => (currentState, Result("", MainMenu))
      case _ =>
        parseInput(input) match {
          case Right(theatre) =>
            val updatedState =
              currentState.copy(theatre = Some(theatre))
            (
              updatedState,
              Result(
                s"A ${theatre.seatingPlan.capacity}-seat seating map has been defined.$LS",
                MainMenu
              )
            )
          case Left(errorMessage) =>
            (
              currentState,
              Result(
                s"""Invalid input format. Please use: [Rows] [Cols]
                   |Specific error: $errorMessage
                   |""".stripMargin,
                this
              )
            )
        }
    }
  }

  private def parseInput(input: String): Either[String, MovieTheatre] = {
    val splitInput = input.trim.split(" ").map(_.trim)
    if (splitInput.length != 2)
      Left("Need two numbers to define a seating map.")
    else
      try {
        Right(
          MovieTheatre(
            RectangularSeatingMap(splitInput(0).toInt, splitInput(1).toInt)
          )
        )
      } catch {
        case e: IllegalArgumentException => Left(e.getMessage)
      }
  }
}
