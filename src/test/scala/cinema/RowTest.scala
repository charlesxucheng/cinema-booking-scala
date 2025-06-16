package cinema

import org.scalatest.matchers.should.Matchers.*

class RowTest extends UnitSpec {

  "A Row" when {
    "created" should {
      "have all its seats available" in {
        val testData = Table(
          "Number of Seats",
          10,
          20,
          80,
          Row.maxRowId
        )
        forAll(testData) { (seatCount: Int) =>
          Row(1, seatCount).availableSeats.seatCount shouldBe seatCount
        }
      }
    }
    "given a non-positive seat count" should {
      "not be created" in {
        val testData = Table(
          "Seat Count",
          0,
          -1,
          -2,
          Int.MinValue
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
          (2, 1.5f),
          (4, 2.5f),
          (10, 5.5f),
          (20, 10.5f),
          (100, 50.5f)
        )
        forAll(testData) { (numberOfSeats: Int, midPoint: Float) =>
          Row(1, numberOfSeats).midPoint shouldBe midPoint
        }
      }
    }
    "created with no name given" should {
      "default its name to A-Z and AA to AZ and so on" in {
        val testData = Table(
          ("id", "name"),
          (1, "A"),
          (2, "B"),
          (25, "Y"),
          (26, "Z"),
          (27, "AA"),
          (28, "AB"),
          (51, "AY"),
          (52, "AZ"),
          (53, "BA"),
          (78, "BZ"),
          (79, "CA"),
          (99, "CU"),
          (104, "CZ"),
          (702, "ZZ")
        )
        forAll(testData) { (id: Int, name: String) =>
          Row(id, 20).name shouldBe name
        }
      }
    }
  }
}
