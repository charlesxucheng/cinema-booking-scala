package cinema

import cinema.Row.SeatBlocks
import cinema.SeatAllocationStrategy.*
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.*

class DefaultSeatAllocationStrategyTest extends UnitSpec {

  "A DefaultSeatAllocationStrategy" when {
    "given a non-positive number of seats to allocate" should {
      "not allocate any seats" in {
        val testData = Table(
          "Number of Seats to Allocate",
          0,
          -1,
          -2,
          Int.MinValue
        )
        forAll(testData) { (numberRequested: Int) =>
          val seatingMap = RectangularSeatingMap(5, 5)
          an[IllegalArgumentException] should be thrownBy {
            DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberRequested
            )
          }
        }
      }
    }

    "given a number of seats to allocate which is larger than the cinema hall's available seats" should {
      "not allocate any seats" in {
        val testData = Table(
          "Number of Seats to Allocate",
          26,
          100,
          1001,
          10001
        )
        forAll(testData) { (numberRequested: Int) =>
          val seatingMap = RectangularSeatingMap(5, 5)
          an[IllegalArgumentException] should be thrownBy {
            DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberRequested
            )
          }
        }
      }
    }

    "given a fully available row of seats and a number of seats requested which is smaller than the row's seat count" should {
      "allocate the center seats (with a right bias, i.e. if it cannot be perfectly centered, then it will take 1 more seat to the right than left)" in {
        val testData = Table(
          (
            "Rows",
            "Cols",
            "Number Of Seats To Allocate",
            "Expected Seat Allocation",
            "Remaining Seats"
          ),
          (5, 5, 1, Seq((3, 3)), Seq((1, 2), (4, 5))),
          (5, 5, 2, Seq((3, 4)), Seq((1, 2), (5, 5))),
          (5, 4, 1, Seq((3, 3)), Seq((1, 2), (4, 4))),
          (5, 4, 2, Seq((2, 3)), Seq((1, 1), (4, 4))),
          (10, 20, 2, Seq((10, 11)), Seq((1, 9), (12, 20))),
          (10, 20, 4, Seq((9, 12)), Seq((1, 8), (13, 20))),
          (20, 45, 11, Seq((18, 28)), Seq((1, 17), (29, 45))),
          (10, 20, 19, Seq((2, 20)), Seq((1, 1))),
          (20, 45, 45, Seq((1, 45)), Seq.empty)
        )
        forAll(testData) {
          (
              rows: Int,
              cols: Int,
              numberToAllocate: Int,
              expectedSeats: Seq[(Int, Int)],
              remainingSeats: Seq[(Int, Int)]
          ) =>
            {
              val seatingMap = RectangularSeatingMap(rows, cols)
              val expectedAllocations =
                SeatBlocks.of(expectedSeats)

              val allocationResult = DefaultSeatAllocationStrategy
                .allocateSeats(seatingMap, numberToAllocate)

              allocationResult.allocatedSeats shouldBe Seq(
                AllocatedSeatBlocks(1, expectedAllocations)
              )
              allocationResult.updatedSeatingMap
                .row(1)
                .availableSeats shouldBe SeatBlocks.of(remainingSeats)
            }
        }
      }
    }

    "given a row of seats with one block booked and a number of seats requested which can fit into th next rightful block" should {
      "allocate from the side that is nearer to the center of the row (right side if equally near)" in {
        val testData = Table(
          (
            "Rows",
            "Cols",
            "Existing Allocation",
            "Number Of Seats To Allocate",
            "Expected Seat Allocation",
            "Remaining Seats"
          ),
          (10, 20, Seq((10, 11)), 2, Seq((12, 13)), Seq((1, 9), (14, 20))),
          (10, 20, Seq((10, 14)), 6, Seq((4, 9)), Seq((1, 3), (15, 20))),
          (1, 11, Seq((6, 6)), 3, Seq((7, 9)), Seq((1, 5), (10, 11))),
          (1, 13, Seq((1, 3)), 8, Seq((4, 11)), Seq((12, 13))),
          (1, 12, Seq((4, 12)), 2, Seq((2, 3)), Seq((1, 1)))
        )
        forAll(testData) {
          (
              rows: Int,
              cols: Int,
              existingAllocation: Seq[(Int, Int)],
              numberToAllocate: Int,
              expectedSeats: Seq[(Int, Int)],
              remainingSeats: Seq[(Int, Int)]
          ) =>
            val rowsAllocated = IndexedSeq(
              Row(1, cols).holdSeatsForBooking(
                SeatBlocks.of(existingAllocation)
              )
            )
            val seatingMap =
              RectangularSeatingMap(rows, cols).holdSeatsForBooking(
                rowsAllocated
              )

            val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberToAllocate
            )

            allocationResult.allocatedSeats shouldBe Seq(
              AllocatedSeatBlocks(1, SeatBlocks.of(expectedSeats))
            )
            allocationResult.updatedSeatingMap
              .row(1)
              .availableSeats shouldBe SeatBlocks.of(remainingSeats)
        }
      }
    }

    "given a row of seats with one block booked and a number of seats requested which cannot fit into the next rightful block" should {
      "allocate multiple blocks starting with the side that is nearer to the center of the row" in {
        val testData = Table(
          (
            "Rows",
            "Cols",
            "Existing Allocation",
            "Number Of Seats To Allocate",
            "Expected Seat Allocation",
            "Remaining Seats"
          ),
          (10, 20, Seq((4, 16)), 5, Seq((17, 20), (3, 3)), Seq((1, 2))),
          (1, 20, Seq((4, 16)), 7, Seq((17, 20), (1, 3)), Seq.empty),
          (5, 23, Seq((8, 20)), 9, Seq((1, 7), (21, 22)), Seq((23, 23)))
        )
        forAll(testData) {
          (
              rows: Int,
              cols: Int,
              existingAllocation: Seq[(Int, Int)],
              numberToAllocate: Int,
              expectedSeats: Seq[(Int, Int)],
              remainingSeats: Seq[(Int, Int)]
          ) =>
            val rowsAllocated = IndexedSeq(
              Row(1, cols).holdSeatsForBooking(
                SeatBlocks.of(existingAllocation)
              )
            )

            val seatingMap =
              RectangularSeatingMap(rows, cols).holdSeatsForBooking(
                rowsAllocated
              )

            val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberToAllocate
            )
            allocationResult.allocatedSeats shouldBe Seq(
              AllocatedSeatBlocks(1, SeatBlocks.of(expectedSeats))
            )
            allocationResult.updatedSeatingMap
              .row(1)
              .availableSeats shouldBe SeatBlocks.of(remainingSeats)
        }
      }
    }

    "given a row of seats with multiple blocks booked and a number of seats requested which fits into the row" should {
      "allocate from the remaining blocks based on their distance to the middle" in {

        val testData = Table(
          (
            "Rows",
            "Cols",
            "Existing Allocation",
            "Number Of Seats To Allocate",
            "Expected Seat Allocation"
          ),
          (10, 20, Seq((8, 11), (14, 17)), 5, Seq((12, 13), (5, 7))),
          (1, 20, Seq((5, 6), (7, 9)), 4, Seq((10, 13))),
          (1, 20, Seq((5, 6), (7, 9), (10, 13)), 8, Seq((14, 20), (4, 4))),
          (1, 20, Seq((5, 6), (7, 9), (11, 14)), 3, Seq((10, 10), (15, 16))),
          (
            1,
            20,
            Seq((5, 6), (7, 9), (11, 14), (10, 10), (15, 16)),
            6,
            Seq((17, 20), (3, 4))
          )
        )
        forAll(testData) {
          (
              rows: Int,
              cols: Int,
              existingAllocation: Seq[(Int, Int)],
              numberToAllocate: Int,
              allocatedSeats: Seq[(Int, Int)]
          ) =>
            val rowsAllocated = IndexedSeq(
              Row(1, cols).holdSeatsForBooking(
                SeatBlocks.of(existingAllocation)
              )
            )
            val seatingMap =
              RectangularSeatingMap(rows, cols).holdSeatsForBooking(
                rowsAllocated
              )

            val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberToAllocate
            )

            allocationResult.allocatedSeats shouldBe Seq(
              AllocatedSeatBlocks(1, SeatBlocks.of(allocatedSeats))
            )
        }
      }
    }

    "given multiple row of seats and a number of seats requested which do not fits into one row" should {
      "allocate from the last row onwards" in {
        val testData = Table(
          (
            "Rows",
            "Cols",
            "Existing Allocation",
            "Number Of Seats To Allocate",
            "Expected Seat Allocation"
          ),
          (10, 20, Seq.empty, 30, Seq(Seq((1, 20)), Seq((6, 15)))),
          (10, 20, Seq((5, 15)), 25, Seq(Seq((1, 4), (16, 20)), Seq((3, 18)))),
          (
            10,
            10,
            Seq((1, 3), (4, 9)),
            12,
            Seq(Seq((10, 10)), Seq((1, 10)), Seq((6, 6)))
          ),
          (3, 10, Seq.empty, 30, Seq(Seq((1, 10)), Seq((1, 10)), Seq((1, 10))))
        )
        forAll(testData) {
          (
              rows: Int,
              cols: Int,
              existingAllocation: Seq[(Int, Int)],
              numberToAllocate: Int,
              allocatedSeatNumbers: Seq[Seq[(Int, Int)]]
          ) =>
            val rowsAllocated = IndexedSeq(
              Row(1, cols).holdSeatsForBooking(
                SeatBlocks.of(existingAllocation)
              )
            )
            val seatingMap =
              RectangularSeatingMap(rows, cols).holdSeatsForBooking(
                rowsAllocated
              )

            val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberToAllocate
            )

            val expectedBlocks = (1 to rows)
              .zip(allocatedSeatNumbers.map(SeatBlocks.of))
              .map(p => AllocatedSeatBlocks(p._1, p._2))
            allocationResult._1 shouldBe expectedBlocks
        }
      }
    }

    "given a number of seats requested form a starting position of an empty row that can accommodate the requested number" should {
      "allocate from that position" in {
        val testData = Table(
          (
            "Number Of Seats To Allocate",
            "Starting Row ID",
            "Starting Column ID",
            "Expected Seat Allocation"
          ),
          (5, 4, 4, Seq((4, 8))),
          (1, 1, 1, Seq((1, 1))),
          (10, 2, 1, Seq((1, 10))),
          (2, 3, 4, Seq((4, 5))),
          (1, 1, 10, Seq((10, 10)))
        )
        forAll(testData) {
          (
              numberOfSeatsRequested: Int,
              startingRowId: Int,
              startingColId: Int,
              expectedSeatAllocation: Seq[(Int, Int)]
          ) =>
            val seatingMap = RectangularSeatingMap(10, 10)
            val result = DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberOfSeatsRequested,
              startingRowId,
              startingColId
            )
            result.allocatedSeats shouldBe Seq(
              AllocatedSeatBlocks(
                startingRowId,
                SeatBlocks.of(expectedSeatAllocation)
              )
            )
        }
      }
    }

    "given a number of seats requested and a starting position of an empty row that cannot accommodate the requested number" should {
      "allocate seats from that row and the row(s) before the starting row" in {
        val testData = Table(
          (
            "Number Of Seats To Allocate",
            "Starting Row ID",
            "Starting Column ID",
            "Expected Seat Allocation in Starting Row",
            "Expected Seat Allocation in the Row Before Starting Row"
          ),
          (10, 4, 4, Seq((4, 10)), Seq((5, 7))),
          (20, 1, 1, Seq((1, 10)), Seq((1, 10))),
          (15, 2, 2, Seq((2, 10)), Seq((3, 8)))
        )

        forAll(testData) {
          (
              numberOfSeatsRequested: Int,
              startingRowId: Int,
              startingColId: Int,
              expectedSeatAllocationInStartingRow: Seq[(Int, Int)],
              expectedSeatAllocationInRowBeforeStartingRow: Seq[(Int, Int)]
          ) =>
            val seatingMap = RectangularSeatingMap(10, 10)
            val result = DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberOfSeatsRequested,
              startingRowId,
              startingColId
            )
            result.allocatedSeats shouldBe Seq(
              AllocatedSeatBlocks(
                startingRowId,
                SeatBlocks.of(expectedSeatAllocationInStartingRow)
              ),
              AllocatedSeatBlocks(
                startingRowId + 1,
                SeatBlocks.of(expectedSeatAllocationInRowBeforeStartingRow)
              )
            )
        }
      }
    }

    "given a number of seats requested and a starting position with some seats to the right of the starting position are already allocated but enough seats are available" should {
      "allocate from the starting position successfully" in {
        val testData = Table(
          (
            "Rows",
            "Cols",
            "Starting Row ID",
            "Starting Column ID",
            "Number Of Seats To Allocate",
            "Existing Allocation",
            "Expected Seat Allocation"
          ),
          (10, 20, 5, 2, 5, Seq((8, 11), (14, 17)), Seq((2, 6))),
          (10, 20, 2, 2, 4, Seq((6, 20)), Seq((2, 5))),
          (1, 10, 1, 1, 8, Seq((10, 10)), Seq((1, 8))),
          (1, 10, 1, 1, 8, Seq((9, 10)), Seq((1, 8))),
          (1, 20, 1, 4, 3, Seq((7, 9), (11, 14), (15, 20)), Seq((4, 6)))
        )

        forAll(testData) {
          (
              rows: Int,
              cols: Int,
              startingRowId: Int,
              startingColId: Int,
              numberToAllocate: Int,
              existingAllocation: Seq[(Int, Int)],
              allocatedSeats: Seq[(Int, Int)]
          ) =>
            val rowsAllocated = IndexedSeq(
              Row(startingRowId, cols).holdSeatsForBooking(
                SeatBlocks.of(existingAllocation)
              )
            )
            val seatingMap =
              RectangularSeatingMap(rows, cols).holdSeatsForBooking(
                rowsAllocated
              )

            val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(
              seatingMap,
              numberToAllocate,
              startingRowId,
              startingColId
            )

            allocationResult.allocatedSeats shouldBe Seq(
              AllocatedSeatBlocks(startingRowId, SeatBlocks.of(allocatedSeats))
            )
        }
      }
    }

    "given a number of seats requested and a starting position with some seats to the right of the starting position are already allocated but not enough seats are available" should {
      "fail to allocate seats" in {
        val testData = Table(
          (
            "Rows",
            "Cols",
            "Starting Row ID",
            "Starting Column ID",
            "Number Of Seats To Allocate",
            "Existing Allocation"
          ),
          (10, 20, 5, 2, 10, Seq((8, 11), (14, 17))),
          (10, 20, 2, 2, 5, Seq((6, 20))),
          (1, 10, 1, 1, 8, Seq((7, 10))),
          (1, 20, 1, 4, 12, Seq((11, 14), (15, 20)))
        )

        forAll(testData) {
          (
              rows: Int,
              cols: Int,
              startingRowId: Int,
              startingColId: Int,
              numberToAllocate: Int,
              existingAllocation: Seq[(Int, Int)]
          ) =>
            val rowsAllocated = IndexedSeq(
              Row(startingRowId, cols).holdSeatsForBooking(
                SeatBlocks.of(existingAllocation)
              )
            )
            val seatingMap =
              RectangularSeatingMap(rows, cols).holdSeatsForBooking(
                rowsAllocated
              )

            an[IllegalArgumentException] should be thrownBy {
              DefaultSeatAllocationStrategy.allocateSeats(
                seatingMap,
                numberToAllocate,
                startingRowId,
                startingColId
              )
            }
        }
      }
    }

    "some seats were allocated" should {
      "have available seats reduced by the number allocated" in {
        val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
        val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)

        forAll(validRows, validCols) { (rows, cols) =>
          val seatingMap = RectangularSeatingMap(rows, cols)
          val numberToAllocate = Gen.choose(1, cols * rows).sample.get
          val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(
            seatingMap,
            numberToAllocate
          )
          allocationResult.updatedSeatingMap.availableSeatCount should be(
            seatingMap.availableSeatCount - numberToAllocate
          )
        }
      }
    }

    "some seats were allocated from a single row only" should {
      "have the correct remaining seats in that row" in {
        val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
        val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)

        forAll(validRows, validCols) { (rows, cols) =>
          val seatingMap = RectangularSeatingMap(rows, cols)
          val numberToAllocate = Gen.choose(1, cols).sample.get
          val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(
            seatingMap,
            numberToAllocate
          )
          val allocatedSeats: SeatBlocks =
            allocationResult.allocatedSeats.head.seatBlocks

          allocationResult.updatedSeatingMap
            .row(1)
            .availableSeats shouldBe SeatBlocks.of(
            Seq((1, cols))
          ) -- allocatedSeats
        }
      }
    }

    "some seats were allocated from multiple rows" should {
      "have the correct number of remaining seats in each affected row" in {
        val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
        val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)

        forAll(validRows, validCols) { (rows, cols) =>
          val seatingMap = RectangularSeatingMap(rows, cols)
          val numberToAllocate = Gen.choose(1, cols * rows).sample.get
          val allocationResult = DefaultSeatAllocationStrategy.allocateSeats(
            seatingMap,
            numberToAllocate
          )
          val firstNonFullRow = numberToAllocate / cols + 1
          val firstNonFullRowSeatAllocation = numberToAllocate % cols

          val allocatedSeats: SeatBlocks =
            allocationResult.allocatedSeats.head.seatBlocks

          for (row <- 1 until firstNonFullRow) {
            allocationResult.updatedSeatingMap
              .row(row)
              .availableSeats
              .seatCount shouldBe 0
          }

          allocationResult.updatedSeatingMap
            .row(firstNonFullRow)
            .availableSeats
            .seatCount shouldBe cols - firstNonFullRowSeatAllocation
        }
      }
    }

    "given a series of seat allocation requests" should {
      "allocate seats correctly across rows" in {
        val initialSeatingMap = RectangularSeatingMap(8, 40)
        val firstAllocationResult =
          DefaultSeatAllocationStrategy.allocateSeats(initialSeatingMap, 30)
        val secondAllocationResult = DefaultSeatAllocationStrategy
          .allocateSeats(firstAllocationResult.updatedSeatingMap, 30)
        val thirdAllocationResult = DefaultSeatAllocationStrategy.allocateSeats(
          secondAllocationResult.updatedSeatingMap,
          4
        )
        firstAllocationResult.allocatedSeats shouldBe Seq(
          AllocatedSeatBlocks(1, SeatBlocks.of(Seq((6, 35))))
        )
        secondAllocationResult.allocatedSeats shouldBe Seq(
          AllocatedSeatBlocks(1, SeatBlocks.of(Seq((1, 5), (36, 40)))),
          AllocatedSeatBlocks(2, SeatBlocks.of(Seq((11, 30))))
        )
        thirdAllocationResult.allocatedSeats shouldBe Seq(
          AllocatedSeatBlocks(2, SeatBlocks.of(Seq((31, 34))))
        )
      }
    }
  }
}
