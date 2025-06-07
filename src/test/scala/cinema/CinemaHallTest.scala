package cinema

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration.*

class CinemaHallTest
    extends UnitSpec
    with TableDrivenPropertyChecks
    with ScalaCheckPropertyChecks {

  "A CinemaHall" when {
    "created without min intermission duration" should {
      "use the minimum intermission duration" in {
        val seatingMap = RectangularSeatingMap(5, 5)
        val hall = CinemaHall(seatingMap)

        hall.minimumIntermission shouldBe CinemaHall.MIN_INTERMISSION_DURATION
      }
    }

    "created without a name" should {
      "use the default name" in {
        val seatingMap = RectangularSeatingMap(5, 5)
        val hall = CinemaHall(seatingMap)

        hall.name shouldBe CinemaHall.DEFAULT_NAME
      }

      "not allow negative or zero intermission time" in {
        val seatingMap = RectangularSeatingMap(5, 5)
        val invalidIntermissions = Table(
          "intermission",
          -1.minutes,
          0.minutes,
          -100.minutes
        )

        forAll(invalidIntermissions) { intermission =>
          an[IllegalArgumentException] should be thrownBy {
            CinemaHall("test", intermission, seatingMap)
          }
        }
      }
    }
  }

  it should {
    "correctly initialize with valid parameters" in {
      val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
      val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)
      val validIntermission = Gen.choose(1, 120).map(_.minutes)
      val hallNames = Gen.alphaStr.filter(_.nonEmpty)

      forAll(validRows, validCols, validIntermission, hallNames) {
        (rows, cols, intermission, name) =>
          val seatingMap = RectangularSeatingMap(rows, cols)
          val hall = CinemaHall(name, intermission, seatingMap)

          hall.name shouldBe name
          hall.minimumIntermission shouldBe intermission
          hall.seatingPlan shouldBe seatingMap
      }
    }

    "have the same capacity as its seating map" in {
      val validRows = Gen.choose(1, RectangularSeatingMap.maxRows)
      val validCols = Gen.choose(1, RectangularSeatingMap.maxCols)
      val validIntermission = Gen.choose(1, 120).map(_.minutes)
      val hallNames = Gen.alphaStr.filter(_.nonEmpty)

      forAll(validRows, validCols, validIntermission, hallNames) {
        (rows, cols, intermission, name) =>
          val seatingMap = RectangularSeatingMap(rows, cols)
          val hall = CinemaHall(name, intermission, seatingMap)

          hall.capacity shouldBe (rows * cols)
      }
    }
  }
}
