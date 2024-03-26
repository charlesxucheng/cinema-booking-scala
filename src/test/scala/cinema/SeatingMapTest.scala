package cinema

import cinema.Row.SeatBlocks
import cinema.Row.SeatBlocks.*
import org.apache.commons.lang3.Range
import org.scalatest.matchers.should.Matchers.*

class SeatingMapTest extends UnitSpec {

  "A Rectangular SeatingMap" should {
    "have a capacity that is equal to its number of rows times number of columns" in {
      RectangularSeatingMap(5, 5).capacity shouldBe 25
    }
  }

  it when {
    "created" should {
      "have all seats available" in {
        val seatingMap = RectangularSeatingMap(6, 10)
        seatingMap.capacity shouldBe seatingMap.availableSeatCount
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
        SeatBlocks.fromPairs(ranges).seatCount shouldBe expectedSize
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
        an[IllegalArgumentException] should be thrownBy SeatBlocks.fromPairs(ranges)
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
        an [IllegalArgumentException] should be thrownBy SeatBlocks.fromPairs(ranges)
      }
    }
  }

  "The difference between SeatBlocks A and B" should {
    "be the ranges that is in A but not in B" in {
      val testData = Table(
        ("SeatBlocks A", "SeatBlocks B", "Diff"),
        (Seq((1, 20)), Seq((1, 10)), Seq((11, 20))),
        (Seq((1, 20)), Seq((1, 20)), Seq.empty),
        (Seq((4, 8)), Seq((8, 8), (4, 5)), Seq((6, 7))),
        (Seq((1, 26)), Seq((1, 10), (12, 14), (21, 25)), Seq((11, 11), (15, 20), (26, 26))),
      )

      forAll(testData) { (first: Seq[(Int, Int)], second: Seq[(Int, Int)], diff: Seq[(Int, Int)]) =>
        SeatBlocks.fromPairs(first) -- SeatBlocks.fromPairs(second) shouldBe SeatBlocks.fromPairs(diff)
      }
    }
  }

  "A Row" when {
    "created" should {
      "have all its seats available" in {
        val size = 80 //TODO: Property based test
        Row(1, "ABC", size).availableSeats.seatCount shouldBe size
      }
    }
    "given a non-positive seat count" should {
      "not be created" in {
        val testData = Table(
          "Seat Count",
          0, -1, -2, Int.MinValue
        )
        forAll(testData) { (seatCount: Int) =>
          an[IllegalArgumentException] should be thrownBy Row(1, "R", seatCount)
        }
      }
    }
    "have an odd number of seats" should {
      "have a middle point that is between the center two seats" in {
        val testData = Table(
          ("Number of Seats", "Middle Point"),
          (2, 2),
          (4, 3),
          (10, 6),
          (20, 11),
          (100, 51)
        )
        forAll(testData) { (numberOfSeats: Int, midPoint: Int) =>
          Row(1, numberOfSeats).midPoint shouldBe midPoint
        }
      }
    }
  }

}
