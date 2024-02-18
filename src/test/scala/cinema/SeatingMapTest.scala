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
        (Seq(Range.of(1, 1)), 1),
        (Seq(Range.of(2, 6)), 5),
        (Seq(Range.of(10, 20), Range.of(1, 5)), 16),
        (Seq(Range.of(4, 7), Range.of(10, 13), Range.of(21, 40)), 28)
      )
      forAll(testData) { (ranges: Seq[Range[Integer]], expectedSize: Int) =>
        SeatBlocks(ranges).seatCount shouldBe expectedSize
      }
    }

    "not allow ranges involving non-positive numbers" in {
      val testData = Table(
        "Ranges",
        Seq(Range.of(-5, -1)),
        Seq(Range.of(1, -1)),
        Seq(Range.of(1, 10), Range.of(-5, 0)),
        Seq(Range.of(-10, -5), Range.of(10, 11))
      )
      forAll(testData) { (ranges: Seq[Range[Integer]]) =>
        an[IllegalArgumentException] should be thrownBy SeatBlocks(ranges)
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
  }

}
