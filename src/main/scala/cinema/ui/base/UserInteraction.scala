package cinema.ui.base

import cats.data.State
import cinema.ui.base.UserInteraction.{Input, Result}

object UserInteraction {
  type Input = String
  type UserInteractionHandler[S] = Input => Result[S]
  type NextInteraction[S] = Either[Exit, UserInteraction[S]]
  private type Output = String

  case class Result[S](
      outputMessage: Output,
      interaction: NextInteraction[S]
  )

  object Result {
    def apply[S](message: Output, interaction: UserInteraction[S]): Result[S] =
      new Result(message, Right(interaction))

    def apply[S](message: Output, exit: Exit): Result[S] =
      new Result(message, Left(exit))
  }
}

// User interaction that carries application state S
trait UserInteraction[S] {
  def getPrompt: String
  def getPrompt(state: S): String = getPrompt
  def handleInput(input: Input): State[S, Result[S]]
  def invalidInputMessage(input: Input) =
    s"Input \"$input\" is invalid.$LS"
}
