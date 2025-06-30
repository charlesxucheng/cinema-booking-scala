package cinema

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.*

class RectangularSeatingMapTest extends UnitSpec {

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

    "return a row given a valid row ID" in {
      val seatingMap = RectangularSeatingMap(5, 5)
      seatingMap.row(1) shouldBe Row(1, 5)
    }

    "return an error if an invalid row ID is given" in {
      val seatingMap = RectangularSeatingMap(5, 5)
      an[IllegalArgumentException] should be thrownBy seatingMap.row(0)
      an[IllegalArgumentException] should be thrownBy seatingMap.row(6)
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

        forAll(validRows, validCols) { (rows, cols) =>
          {
            val allocationResult = DefaultSeatAllocationStrategy
              .allocateSeats(
                RectangularSeatingMap(rows, cols),
                Math.min(
                  2,
                  RectangularSeatingMap(rows, cols).availableSeatCount
                )
              )
            val allocatedSeats = allocationResult.allocatedSeats
            val seatingMap = allocationResult.updatedSeatingMap

            seatingMap
              .confirmSeatsForBooking(allocatedSeats)
              .seatingMapBeforeHold shouldBe None
          }
        }
      }
    }
  }

}
