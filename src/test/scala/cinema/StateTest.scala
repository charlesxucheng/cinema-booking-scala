package cinema

import cats.effect.IO
import org.scalatest.matchers.should.Matchers.*

import scala.language.postfixOps


class StateTest extends UnitSpec {
  "MenuSelectionState" when {
    "receives a input of 4" should {
      "end the application" in {
        val state = MenuSelectionState
        val output = state.processInput("4")
        output shouldBe a [IO[Unit]]
      }
    }
    
    "receives a input that is not 1 to 4" should {
      "return the same state" in {
      }
    }
  }
  
}
