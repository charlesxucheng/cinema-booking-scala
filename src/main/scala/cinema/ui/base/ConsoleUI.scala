package cinema.ui.base

import cats.data.StateT
import cats.effect.std.Console
import cats.effect.{ExitCode, IO}
import cinema.ui.base.UserInteraction.NextInteraction

def LS = System.lineSeparator()

trait ConsoleUI[S](console: Console[IO]) {
  def start(
      initialInteraction: UserInteraction[S]
  )(using Empty[S]): IO[ExitCode] = {
    def loop(
        currentInteraction: UserInteraction[S],
        currentState: S
    ): IO[ExitCode] =
      processInput(currentInteraction).run(currentState).flatMap {
        case (newState: S, interaction) =>
          interaction match {
            case Left(ec) => exit(ec)
            case Right(nextInteraction) =>
              loop(nextInteraction, newState)
          }
      }

    for {
      _ <- displayWelcomeMessage()
      exit <- loop(initialInteraction, Empty.apply)
    } yield exit
  }

  private def processInput(
      interaction: UserInteraction[S]
  ): StateT[IO, S, NextInteraction[S]] = {
    for {
      _ <- StateT.liftF(console.println(interaction.getPrompt))
      input <- StateT.liftF(console.readLine)
      result <- StateT { (currentState: S) =>
        val (s, a) = interaction.handleInput(input).run(currentState).value
        IO.pure((s, a))
      }
      _ <- StateT.liftF(
        if (result.outputMessage.nonEmpty)
          console.println(result.outputMessage)
        else IO.unit
      )
    } yield result.interaction
  }

  def exit(exit: Exit): IO[ExitCode] =
    for {
      _ <- console.println(exit.exitMessage)
    } yield ExitCode.Success

  def displayWelcomeMessage(): IO[Unit]

}
