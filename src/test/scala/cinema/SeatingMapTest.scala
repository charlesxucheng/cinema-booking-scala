package cinema

import org.scalatest.matchers.should.Matchers.*

class SeatingMapTest extends UnitSpec {

  "A Rectangular Seating" should {
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

}
