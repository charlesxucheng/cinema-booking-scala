package cinema

import cinema.SeatingMapView.{*, given}
import org.scalacheck.Gen
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers.*

class SeatingMapViewTest extends UnitSpec {

  private val threshold = longHeader.length / 2

  "A SeatingMapView for RectangularSeatingMap" when {
    s"the max number of seats of rows in a Seating Map is more than $threshold" should {
      s"display the first line as $longHeader centered" in {
        RectangularSeatingMap(5, 25).viewAsSingleString should startWith(
          "                    S C R E E N                    " + LS
        )
      }
      s"the max number of seats of rows in a Seating Map is less than $threshold" should {
        s"display the first line as $shortHeader centered" in {
          RectangularSeatingMap(5, 4).viewAsSingleString should startWith(
            "    S    " + LS
          )
        }
      }
      s"the max number of seats of rows in a Seating Map is equal to $threshold" should {
        s"display the first line as $longHeader with no paddings" in {
          RectangularSeatingMap(5, 5).viewAsSingleString should startWith(
            longHeader + LS
          )
        }
      }
    }
    
    "have 1 to 99 seats per row" should {
      "display 1 space between two seats" in {
        val cols = Gen.choose(1, 99)
        forAll(cols) { col =>
          RectangularSeatingMap(5, col).viewAsSingleString should include(" ." * col)
        }
      }
    }
    
    "have more than 99 seats per row" should {
      "display 2 spaces between two seats" in {
        val cols = Gen.choose(100, RectangularSeatingMap.maxCols)
        forAll(cols) { col =>
          RectangularSeatingMap(5, col).viewAsSingleString should include("  ." * col)
        }
      }
    }
  }

  "Every line in the SeatingMapView" should {
    "have the same length" in {
      val testData = Table(
        ("rows", "cols", "expectedLength"),
        (5, 25, 51),
        (4, 4, 9),
        (6, 5, 11),
        (20, 10, 21),
        (27, 10, 22)
      )
      forAll(testData) { (rows: Int, cols: Int, expectedLength: Int) =>
        {
          val viewContent =
            RectangularSeatingMap(rows, cols).viewAsMultiPartContent
          viewContent.header.length shouldBe expectedLength
          viewContent.separator.length shouldBe expectedLength
          Inspectors.forAll(viewContent.seats) { line =>
            line.length shouldBe expectedLength
          }
        }
      }
    }
  }

  "An empty SeatingMap" should {
    "display all seats as available" in {
      RectangularSeatingMap(5, 5).viewAsMultiPartContent.seats shouldBe Seq(
        "E . . . . .",
        "D . . . . .",
        "C . . . . .",
        "B . . . . .",
        "A . . . . ."
      )
    }
  }

  "A SeatingMap with some bookings" should {
    "show seats held by a booking as held" in {
      val initialSeatingMap = RectangularSeatingMap(5, 5)
      val allocationResult =
        DefaultSeatAllocationStrategy.allocateSeats(initialSeatingMap, 2)

      allocationResult.updatedSeatingMap.viewAsMultiPartContent.seats shouldBe Seq(
        "E . . . . .",
        "D . . . . .",
        "C . . . . .",
        "B . . . . .",
        "A . . o o ."
      )
    }
  }
  
  "A SeatingMap that has more than 26 rows" should {
    "display an extra space after row name for single letter row names" in {
      val rows = RectangularSeatingMap(27, 5).viewAsMultiPartContent.seats
      rows.head shouldBe "AA . . . . ."
      rows.last shouldBe "A  . . . . ."
    }
  }

}
