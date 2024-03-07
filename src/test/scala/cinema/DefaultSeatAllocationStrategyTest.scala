package cinema

import cinema.Row.SeatBlocks.*
import SeatAllocationStrategy.*
import org.scalatest.matchers.should.Matchers.shouldBe

class DefaultSeatAllocationStrategyTest extends UnitSpec {

  "A DefaultSeatAllocationStrategy" when {
    "Given a fully available row of seats and a number of seats requested which is smaller than the row's seat count" should {
      "allocate the center seats (with a right bias, i.e. if it cannot be perfectly centered, then it will take 1 more seat to the right than left)" in {
         val seatingMap = RectangularSeatingMap(10, 20)
         DefaultSeatAllocationStrategy.allocateSeats(seatingMap, 2) shouldBe Seq(AllocatedSeatBlocks(10, fromPairs(Seq((10, 11)))))
      }

    }
  }

}
