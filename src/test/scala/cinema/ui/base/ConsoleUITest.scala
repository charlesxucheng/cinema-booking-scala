package cinema.ui.base

import cats.data.State
import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO}
import cinema.UnitSpec
import cinema.ui.MockConsole
import cinema.ui.base.UserInteraction.{Input, Result}

class ConsoleUITest extends UnitSpec {
  private val testExitMessage = "Goodbye from Test!"
  private val testPrompt = "Test Prompt:"
  private val testWelcomeMessage = "Welcome from Test!"

  class EndingUserInteraction extends UserInteraction[String] {
    override def getPrompt: String = testPrompt

    override def handleInput(
        input: Input
    ): State[String, Result[String]] =
      State.pure(UserInteraction.Result("Echo: " + input, DummyExit))
  }

  class StartingUserInteraction extends UserInteraction[String] {
    override def getPrompt: String = testPrompt

    override def handleInput(
        input: Input
    ): State[String, Result[String]] =
      State.pure(
        UserInteraction.Result("Echo: " + input, new EndingUserInteraction())
      )
  }

  class TestConsoleUI(console: MockConsole) extends ConsoleUI[String](console) {
    override def displayWelcomeMessage(): IO[Unit] = IO {
      console.println(testWelcomeMessage)
    }
  }

  case object DummyExit extends Exit {
    override val exitMessage: String = testExitMessage
  }

  object EndingUserInteraction {
    given Empty[String] with
      def empty: String = ""
  }

  "Dummy ConsoleUI" should {
    "print the exit message before exit" in {
      val mockConsole = MockConsole(Nil)
      val ui = new TestConsoleUI(mockConsole)
      ui.exit(DummyExit).unsafeRunSync() mustBe ExitCode.Success
      mockConsole.getOutputLines.last.trim mustBe testExitMessage
    }

    "display welcome, prompt, echo input, and exit on start" in {
      import EndingUserInteraction.given

      val testInput = "hello"
      val mockConsole = MockConsole(List(testInput))
      val ui = new TestConsoleUI(mockConsole)
      val exitCode = ui.start(new EndingUserInteraction()).unsafeRunSync()
      exitCode mustBe ExitCode.Success
      val output = mockConsole.getOutputLines.mkString("")
      output must include(testWelcomeMessage)
      output must include(testPrompt)
      output must include(s"Echo: $testInput")
    }

    "process multiple inputs in sequence" in {
      import EndingUserInteraction.given

      val testInputs = List("first", "second")
      val mockConsole = MockConsole(testInputs)
      val ui = new TestConsoleUI(mockConsole)
      val exitCode = ui.start(new StartingUserInteraction()).unsafeRunSync()
      exitCode mustBe ExitCode.Success
      val output = mockConsole.getOutputLines.mkString("")
      output must include(testWelcomeMessage)
      output must include(testPrompt) // Two prompts for two inputs
      output must include("Echo: first")
      output must include("Echo: second")
      mockConsole.getOutputLines.last.trim mustBe testExitMessage
    }
  }
}
