package cinema

import cinema.Row.{SeatBlock, SeatBlocks}
import cinema.Row.SeatBlocks.*
import cinema.SeatAllocationStrategy.*
import org.scalatest.matchers.should.Matchers.shouldBe

class DefaultSeatAllocationStrategyTest extends UnitSpec {

  "A DefaultSeatAllocationStrategy" when {
    "given a fully available row of seats and a number of seats requested which is smaller than the row's seat count" should {
      "allocate the center seats (with a right bias, i.e. if it cannot be perfectly centered, then it will take 1 more seat to the right than left)" in {
        val testData = Table(
          ("rows", "cols", "numberToAllocate", "allocatedSeats"),
          (10, 20, 2, Seq((10, 11))),
          (10, 20, 4, Seq((9, 12))),
          (20, 45, 11, Seq((18, 28))),
          (10, 20, 19, Seq((2, 20))),
          (20, 45, 45, Seq((1, 45)))
        )
        forAll(testData) { (rows: Int, cols: Int, numberToAllocate: Int, allocatedSeatNumbers: Seq[(Int, Int)]) => {
          val seatingMap = RectangularSeatingMap(rows, cols)
          val allocatedSeats = fromPairs(allocatedSeatNumbers)
          val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(seatingMap, numberToAllocate)
          allocationResult._1 shouldBe Seq(AllocatedSeatBlocks(rows, allocatedSeats))
          allocationResult._2.availableSeatCount shouldBe allocationResult._2.capacity - numberToAllocate
          allocationResult._2.seats(rows - 1).availableSeats shouldBe fromPairs(Seq((1, cols))) -- allocatedSeats
        }
        }
      }
    }
    "given a row of seats with one block booked and a number of seats requested which can fit into th next rightful block" should {
      "allocate from the side that is nearer to the center of the row (right side if equally near)" in {
        val testData = Table(
          ("rows", "cols", "existingAllocation", "numberToAllocate", "allocatedSeats"),
          (10, 20, Seq((10, 11)), 2, Seq((12, 13))),
          (10, 20, Seq((10, 14)), 6, Seq((4, 9))),
          (1, 11, Seq((6, 6)), 3, Seq((7, 9))),
          (1, 13, Seq((1, 3)), 8, Seq((4, 11))),
          (1, 12, Seq((4, 12)), 2, Seq((2, 3)))
        )
        forAll(testData) { (rows: Int, cols: Int, existingAllocation: Seq[(Int, Int)], numberToAllocate: Int, allocatedSeatNumbers: Seq[(Int, Int)]) =>
          val initialSeatingMap = RectangularSeatingMap(rows, cols)
          val rowsAllocated = IndexedSeq(Row(rows, cols).assignSeats(fromPairs(existingAllocation)))
          val startSeatingMap = initialSeatingMap.bookSeats(rowsAllocated)
          val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(startSeatingMap, numberToAllocate)
          allocationResult._1 shouldBe Seq(AllocatedSeatBlocks(rows, fromPairs(allocatedSeatNumbers)))
        }
      }
    }
    "given a row of seats with one block booked and a number of seats requested which cannot fit into the next rightful block" should {
      "allocate multiple blocks starting with the side that is nearer to the center of the row" in {
        val testData = Table(
          ("rows", "cols", "existingAllocation", "numberToAllocate", "allocatedSeats"),
          (10, 20, Seq((4, 16)), 5, Seq((17, 20), (3, 3))),
          (1, 20, Seq((4, 16)), 7, Seq((17, 20), (1, 3))),
          (5, 23, Seq((8, 20)), 9, Seq((1, 7), (21, 22)))
        )
        forAll(testData) { (rows: Int, cols: Int, existingAllocation: Seq[(Int, Int)], numberToAllocate: Int, allocatedSeatNumbers: Seq[(Int, Int)]) =>
          val initialSeatingMap = RectangularSeatingMap(rows, cols)
          val rowsAllocated = IndexedSeq(Row(rows, cols).assignSeats(fromPairs(existingAllocation)))
          val startSeatingMap = initialSeatingMap.bookSeats(rowsAllocated)
          val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(startSeatingMap, numberToAllocate)
          allocationResult._1 shouldBe Seq(AllocatedSeatBlocks(rows, fromPairs(allocatedSeatNumbers)))
        }
      }
    }
    "given a row of seats with multiple blocks booked and a number of seats requested which fits into the row" should {
      "allocate from the remaining blocks based on their distance to the middle" in {

        val testData = Table(
          ("rows", "cols", "existingAllocation", "numberToAllocate", "allocatedSeats"),
          (10, 20, Seq((8, 11), (14, 17)), 5, Seq((12, 13), (5, 7))),
          (1, 20, Seq((5, 6), (7, 9)), 4, Seq((10, 13))),
          (1, 20, Seq((5, 6), (7, 9), (10, 13)), 8, Seq((14, 20), (4, 4))),
          (1, 20, Seq((5, 6), (7, 9), (11, 14)), 3, Seq((10, 10), (15, 16))),
          (1, 20, Seq((5, 6), (7, 9), (11, 14), (10, 10), (15, 16)), 6, Seq((17, 20), (3, 4)))
        )
        forAll(testData) { (rows: Int, cols: Int, existingAllocation: Seq[(Int, Int)], numberToAllocate: Int, allocatedSeatNumbers: Seq[(Int, Int)]) =>
          val initialSeatingMap = RectangularSeatingMap(rows, cols)
          val rowsAllocated = IndexedSeq(Row(rows, cols).assignSeats(fromPairs(existingAllocation)))
          val startSeatingMap = initialSeatingMap.bookSeats(rowsAllocated)
          val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(startSeatingMap, numberToAllocate)
          allocationResult._1 shouldBe Seq(AllocatedSeatBlocks(rows, fromPairs(allocatedSeatNumbers)))
        }
      }
    }
  }

}
