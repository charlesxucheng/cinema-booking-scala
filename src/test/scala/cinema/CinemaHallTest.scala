package cinema

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CinemaHallTest extends UnitSpec {

  "A CinemaHall" when {
    "created without a name" should {
      "use the default name" in {
        val seatingMap = RectangularSeatingMap(5, 5)
        val hall = CinemaHall(seatingMap)

        hall.name shouldBe CinemaHall.DEFAULT_NAME
      }
    }
  }

  it should {
    "correctly initialize with valid parameters" in {
      val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
      val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)
      val hallNames = Gen.alphaStr.filter(_.nonEmpty)

      forAll(validRows, validCols, hallNames) { (rows, cols, name) =>
        val seatingMap = RectangularSeatingMap(rows, cols)
        val hall = CinemaHall(name, seatingMap)

        hall.name shouldBe name
        hall.minimumIntermission shouldBe CinemaHall.MIN_INTERMISSION_DURATION
        hall.seatingMap shouldBe seatingMap
      }
    }

    "have the same capacity as its seating map" in {
      val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
      val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)
      val hallNames = Gen.alphaStr.filter(_.nonEmpty)

      forAll(validRows, validCols, hallNames) { (rows, cols, name) =>
        val seatingMap = RectangularSeatingMap(rows, cols)
        val hall = CinemaHall(name, seatingMap)

        hall.capacity shouldBe (rows * cols)
      }
    }
  }
}
