package cinema

import cinema.Row.SeatBlocks
import cinema.Row.SeatBlocks.*
import cinema.SeatAllocationStrategy.*
import org.scalatest.matchers.should.Matchers.shouldBe

class DefaultSeatAllocationStrategyTest extends UnitSpec {

  "A DefaultSeatAllocationStrategy" when {
    "given a fully available row of seats and a number of seats requested which is smaller than the row's seat count" should {
      "allocate the center seats (with a right bias, i.e. if it cannot be perfectly centered, then it will take 1 more seat to the right than left)" in {
        val testData = Table(
          ("rows", "cols", "numberToAllocate", "allocatedSeats"),
          (10, 20, 2, fromPairs(Seq((10, 11)))),
          (10, 20, 4, fromPairs(Seq((9, 12)))),
          (20, 45, 11, fromPairs(Seq((18, 28)))),
          (10, 20, 19, fromPairs(Seq((2, 20)))),
          (20, 45, 45, fromPairs(Seq((1, 45))))
        )
        forAll(testData) { (rows: Int, cols: Int, numberToAllocate: Int, allocatedSeats: SeatBlocks) => {
          val seatingMap = RectangularSeatingMap(rows, cols)
          val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(seatingMap, numberToAllocate)
          allocationResult._1 shouldBe Seq(AllocatedSeatBlocks(rows, allocatedSeats))
          allocationResult._2.availableSeatCount shouldBe allocationResult._2.capacity - numberToAllocate
          allocationResult._2.seats(rows - 1).availableSeats shouldBe fromPairs(Seq((1, cols))) -- allocatedSeats
        }}
      }
    }
    "given a row of seats with one block booked" should {

    }
  }

}
