package cinema

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SeatAllocationStrategyTest extends UnitSpec with ScalaCheckDrivenPropertyChecks {
  "A reference point" when {
    "calculated for a Row with even number of seats" should {
      "never refer to an actual seat" in {
        val evenRowSizes = for (n <- Gen.choose(1, 10000)) yield 2 * n
        forAll(evenRowSizes) { n =>
          SeatAllocationStrategy.refersToASeat(Row(1, n).midPoint) shouldBe false
        }
      }
    }
    "calculated for a Row with odd number of seats" should {
      "always refer to an actual seat" in {
        val oddRowSizes = for (n <- Gen.choose(0, 10000)) yield 2 * n + 1
        forAll(oddRowSizes) { n =>
          SeatAllocationStrategy.refersToASeat(Row(1, n).midPoint) shouldBe true
        }
      }
    }
  }

}
