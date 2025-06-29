package cinema

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
      val testData = Table("rowName", "AZ", "zz", "1", "^%*%", "", " ")
      val seatingMap = RectangularSeatingMap(50, 5)

      forAll(testData) { (rowName: String) =>
        seatingMap.getRowIdByName(rowName) shouldBe None
      }
    }
  }

}
