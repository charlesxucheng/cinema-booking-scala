package cinema

import cinema.Row.SeatBlocks
import org.scalatest.matchers.should.Matchers.*

class RowTest extends UnitSpec {

  "A Row" when {
    "created" should {
      "have all its seats available and none are booked" in {
        val testData = Table(
          "Number of Seats",
          10,
          20,
          80,
          Row.maxRowId
        )
        forAll(testData) { (seatCount: Int) =>
          Row(1, seatCount).availableSeats.seatCount shouldBe seatCount
          Row(1, seatCount).bookedSeats.seatCount shouldBe 0
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

    "given an row Id that is out of the allowed range" should {
      "not be created" in {
        val testData = Table(
          "Row ID",
          -1,
          0,
          Row.minRowId - 1,
          Row.maxRowId + 1,
          Int.MaxValue,
          Int.MinValue
        )
        forAll(testData) { (rowId: Int) =>
          an[IllegalArgumentException] should be thrownBy Row(rowId, 10)
        }
      }

      "given a row name that is more than allowed length" should {
        "not be created" in {
            an[IllegalArgumentException] should be thrownBy Row(1, "A" * 21, 10)
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

    "some seats are held by a booking" should {
      "mark them as booking in progress and unavailable" in {
        val testData = Table(
          ("Seat Blocks", "Expected Available Seats"),
          (Seq(Range.inclusive(1, 1)), 19),
          (Seq(Range.inclusive(1, 5)), 15),
          (Seq(Range.inclusive(1, 10)), 10),
          (Seq(Range.inclusive(1, 15)), 5),
          (Seq(Range.inclusive(1, 20)), 0)
        )

        forAll(testData) {
          (seatBlocks: Seq[Row.SeatBlock], expectedAvailableSeats: Int) =>
            val row = Row(1, 20).holdSeatsForBooking(SeatBlocks(seatBlocks))
            row.bookingInProgressSeats shouldBe SeatBlocks(seatBlocks)
            row.availableSeats.seatCount shouldBe expectedAvailableSeats
        }
      }
    }

    "attempting to confirm a booking for seats held" should {
      "move the seats from held for booking to booked" in {
        val testData = Table(
          "Held Seats",
          Seq(Range.inclusive(3, 6)),
          Seq(Range.inclusive(1, 5)),
          Seq(Range.inclusive(9, 9)),
          Seq(Range.inclusive(1, 20))
        )

        forAll(testData) { (heldSeats: Seq[Row.SeatBlock]) =>
          val seatBlocks = SeatBlocks(heldSeats)
          val row = Row(1, 20)
            .holdSeatsForBooking(seatBlocks)
            .confirmBooking(seatBlocks)
          row.bookedSeats shouldBe seatBlocks
          row.bookingInProgressSeats shouldBe SeatBlocks.empty
        }
      }
    }

    "attempt to book seats that are already booked" should {
      "fail" in {
        val testData = Table(
          ("Already Booked Seats", "Seats To Book"),
          (Seq(Range.inclusive(3, 6)), Seq(Range.inclusive(3, 6))),
          (Seq(Range.inclusive(1, 5)), Seq(Range.inclusive(2, 5))),
          (Seq(Range.inclusive(4, 9)), Seq(Range.inclusive(9, 10))),
          (Seq(Range.inclusive(15, 20)), Seq(Range.inclusive(10, 17))),
          (Seq(Range.inclusive(1, 5)), Seq(Range.inclusive(1, 4))),
        )

        forAll(testData) { (bookedSeats: Seq[Row.SeatBlock], toBook: Seq[Row.SeatBlock]) =>
          val row = Row(1, 20).holdSeatsForBooking(SeatBlocks(bookedSeats))
          an[IllegalArgumentException] should be thrownBy row.holdSeatsForBooking(
            SeatBlocks(toBook)
          )
        }
      }
    }

    "attempting to book seats that have not been held previously" should {
      "fail" in {
        val testData = Table(
          ("Seats Held", "Seats To Book"),
          (Seq(Range.inclusive(3, 6)), Seq(Range.inclusive(8, 11))),
          (Seq(Range.inclusive(1, 5)), Seq(Range.inclusive(5, 10))),
          (Seq(Range.inclusive(9, 9)), Seq(Range.inclusive(1, 10))),
          (Seq(Range.inclusive(1, 20)), Seq(Range.inclusive(8, 10)))
        )

        forAll(testData) {
          (seatsHeld: Seq[Row.SeatBlock], seatsToBook: Seq[Row.SeatBlock]) =>
            val row = Row(1, 20).holdSeatsForBooking(SeatBlocks(seatsHeld))
            an[IllegalArgumentException] should be thrownBy row.confirmBooking(
              SeatBlocks(seatsToBook)
            )
        }
      }
    }

    "attempting to confirm a booking for seats that are already booked" should {
      "fail" in {
        val testData = Table(
          "Seats",
          Seq(Range.inclusive(3, 6)),
          Seq(Range.inclusive(1, 5)),
          Seq(Range.inclusive(9, 9)),
          Seq(Range.inclusive(1, 20))
        )

        forAll(testData) { (seats: Seq[Row.SeatBlock]) =>
          val seatBlocks = SeatBlocks(seats)
          val row = Row(1, 20)
            .holdSeatsForBooking(seatBlocks)
            .confirmBooking(seatBlocks)
          an[IllegalArgumentException] should be thrownBy row.confirmBooking(
            seatBlocks
          )
        }
      }
    }
  }
}
