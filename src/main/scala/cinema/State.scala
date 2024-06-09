package cinema

import cats.effect.IO

type ValidationResult = Either[String, Error]

case class Output(validationResult: ValidationResult, nextState: State)
sealed trait Error
object Error {
  case object InvalidInput extends Error
}


sealed trait State {
  def showMessage: IO[String]

  def processInput(input: String): IO[Output] | IO[Unit]
}

case object MenuSelectionState extends State {
  def showMessage: IO[String] = {
    IO.pure("""
     Welcome to the Cinema Booking System!
     [1] Set Movie & show times
     [2] Define seating map
     [3] Book tickets
     [4] Exit
     Please enter your selection:""")
  }
  def processInput(input: String): IO[Output] | IO[Unit] = {
    input.trim match {
      case "4" => end
      case _ => IO.pure(Output(Right(Error.InvalidInput), MenuSelectionState))
    }
  }

  private def end: IO[Unit] = IO {
    System.exit(0)
  }
}
