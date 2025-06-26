package cinema

import cinema.Row.SeatBlocks
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.*

class SeatingMapTest extends UnitSpec {

  "A SeatingMap" should {
    "return the correct row ID for a given row name" in {
      val testData = Table(
        ("rowName", "rowId"),
        ("A", 1),
        ("C", 3),
        ("E", 5),
        ("AA", 27)
      )

      val seatingMap = RectangularSeatingMap(50, 5)

      forAll(testData) { (rowName: String, rowId: Int) =>
        seatingMap.getRowIdByName(rowName) shouldBe Some(rowId)
      }
    }

    "return the correct row name for a given row ID in lowercase" in {
      val testData = Table(
        ("rowName", "rowId"),
        ("a", 1),
        ("c", 3),
        ("e", 5),
        ("aa", 27)
      )

      val seatingMap = RectangularSeatingMap(50, 5)

      forAll(testData) { (rowName: String, rowId: Int) =>
        seatingMap.getRowIdByName(rowName) shouldBe Some(rowId)
      }
    }

    "return nothing for an invalid row ID" in {
      val testData = Table(
        "rowName", "AZ", "zz", "1", "^%*%", "", " ")
      val seatingMap = RectangularSeatingMap(50, 5)

      forAll(testData) { (rowName: String) =>
        seatingMap.getRowIdByName(rowName) shouldBe None
      }
    }
  }

  "A Rectangular SeatingMap" should {
    "have a capacity that is equal to its number of rows times number of columns" in {
      RectangularSeatingMap(5, 5).capacity shouldBe 25
    }

    "not allow negative or zero number of rows or columns" in {
      val testData = Table(
        ("rows", "cols"),
        (-1, 5),
        (0, 5),
        (5, -1),
        (5, 0),
        (-4, 10),
        (10, -4),
        (Int.MinValue, Int.MinValue),
        (0, 0)
      )
      forAll(testData) { (rows: Int, cols: Int) =>
        an[IllegalArgumentException] should be thrownBy RectangularSeatingMap(
          rows,
          cols
        )
      }
    }

    "not allow rows or columns that exceed the maximum" in {
      val testData = Table(
        ("rows", "cols"),
        (RectangularSeatingMap.maxRows + 1, 5),
        (5, RectangularSeatingMap.maxCols + 1)
      )
      forAll(testData) { (rows: Int, cols: Int) =>
        an[IllegalArgumentException] should be thrownBy RectangularSeatingMap(
          rows,
          cols
        )
      }
    }
  }

  it when {
    "created" should {
      "have all seats available" in {
        val tableData = Table(
          ("rows", "cols"),
          (5, 5),
          (5, 10),
          (10, 5),
          (6, 10),
          (RectangularSeatingMap.maxRows, RectangularSeatingMap.maxCols)
        )
        forAll(tableData) { (rows: Int, cols: Int) =>
          val seatingMap = RectangularSeatingMap(rows, cols)
          seatingMap.availableSeatCount shouldBe seatingMap.capacity
        }
      }
    }

    "Some seats are held for booking" should {
      "save the original map without the held seats" in {
        val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
        val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)

        forAll(validRows, validCols) { (rows, cols) =>
          {
            val initialSeatingMap = RectangularSeatingMap(rows, cols)
            DefaultSeatAllocationStrategy
              .allocateSeats(
                initialSeatingMap,
                Math.min(2, initialSeatingMap.availableSeatCount)
              )
              .updatedSeatingMap
              .seatingMapBeforeHold shouldBe Some(initialSeatingMap)
          }
        }
      }
    }
    
    "The held seats are confirmed" should {
      "discard the original map" in {
        val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
        val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)

        forAll(validRows, validCols) { (rows, cols) => {
          val allocationResult = DefaultSeatAllocationStrategy
            .allocateSeats(
              RectangularSeatingMap(rows, cols),
              Math.min(2, RectangularSeatingMap(rows, cols).availableSeatCount)
            )
          val allocatedSeats = allocationResult.allocatedSeats
          val seatingMap = allocationResult.updatedSeatingMap

          seatingMap.confirmSeatsForBooking(allocatedSeats).seatingMapBeforeHold shouldBe None
        }
        }
      }
    }
  }


  "A non-empty SeatBlocks" should {
    "have size that is equal to the sum of the number of elements its Ranges have" in {
      val testData = Table(
        ("Ranges", "Size"),
        (Seq((1, 1)), 1),
        (Seq((2, 6)), 5),
        (Seq((10, 20), (1, 5)), 16),
        (Seq((4, 7), (10, 13), (21, 40)), 28)
      )
      forAll(testData) { (ranges: Seq[(Int, Int)], expectedSize: Int) =>
        SeatBlocks.of(ranges).seatCount shouldBe expectedSize
      }
    }

    "not allow ranges involving non-positive numbers" in {
      val testData = Table(
        "Ranges",
        Seq((-5, -1)),
        Seq((1, -1)),
        Seq((1, 10), (-5, 0)),
        Seq((-10, -5), (10, 11))
      )
      forAll(testData) { (ranges: Seq[(Int, Int)]) =>
        an[IllegalArgumentException] should be thrownBy SeatBlocks.of(ranges)
      }
    }

    "not have overlapping ranges" in {
      val testData = Table(
        "Ranges",
        Seq((1, 6), (6, 15)),
        Seq((1, 16), (15, 16)),
        Seq((2, 5), (6, 8), (7, 10)),
        Seq((55, 100), (57, 58)),
        Seq((42, 42), (42, 42)),
        Seq((42, 48), (42, 48))
      )
      forAll(testData) { (ranges: Seq[(Int, Int)]) =>
        an[IllegalArgumentException] should be thrownBy SeatBlocks.of(ranges)
      }
    }
  }

  "The difference between SeatBlocks A and B" should {
    "be the ranges that is in A but not in B" in {
      val testData = Table(
        ("SeatBlocks A", "SeatBlocks B", "Diff"),
        (Seq((1, 20)), Seq((1, 10)), Seq((11, 20))),
        (Seq((5, 24)), Seq((24, 24)), Seq((5, 23))),
        (Seq((1, 20)), Seq((1, 20)), Seq.empty),
        (Seq((4, 8)), Seq((8, 8), (4, 5)), Seq((6, 7))),
        (
          Seq((1, 26)),
          Seq((1, 10), (12, 14), (21, 25)),
          Seq((11, 11), (15, 20), (26, 26))
        ),
        (
          Seq((1, 26)),
          Seq((5, 10), (12, 14), (21, 25)),
          Seq((1, 4), (11, 11), (15, 20), (26, 26))
        ),
        (Seq.empty, Seq((12, 15), (3, 6)), Seq.empty),
        (Seq((1, 20)), Seq.empty, Seq((1, 20))),
        (Seq((1, 20), (35, 40)), Seq.empty, Seq((1, 20), (35, 40)))
      )

      forAll(testData) {
        (
            first: Seq[(Int, Int)],
            second: Seq[(Int, Int)],
            diff: Seq[(Int, Int)]
        ) =>
          SeatBlocks.of(first) -- SeatBlocks.of(second) shouldBe SeatBlocks.of(
            diff
          )
      }
    }
  }

}
