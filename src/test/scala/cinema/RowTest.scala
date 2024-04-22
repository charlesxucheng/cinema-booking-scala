package cinema

import org.scalatest.matchers.should.Matchers.*

class RowTest extends UnitSpec {

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
          (2, 1.5F),
          (4, 2.5F),
          (10, 5.5F),
          (20, 10.5F),
          (100, 50.5F)
        )
        forAll(testData) { (numberOfSeats: Int, midPoint: Float) =>
          Row(1, numberOfSeats).midPoint shouldBe midPoint
        }
      }
    }
    "created with no name given" should {
      "default its name to A-Z and AA to AZ" in {
        val testData = Table(
          ("id", "name"),
          (1, "A"),
          (2, "B"),
          (25, "Y"),
          (26, "Z"),
          (27, "AA"),
          (28, "AB"),
          (51, "AY"),
          (52, "AZ")
        )
        forAll(testData) { (id: Int, name: String) => Row(id, 20).name shouldBe name }
      }
    }
  }
}
