package cinema

import cinema.Row.SeatBlocks
import org.scalatest.matchers.should.Matchers.*

class SeatBlocksTest extends UnitSpec {

  private def toSeatBlocks(seq: Seq[(Int, Int)]): SeatBlocks = {
    SeatBlocks(seq.map(Range.inclusive(_, _)))
  }

  "A SeatBlocks" when {
    "subtracted by another SeatBlocks" should {
      "return all the seats that are in the first SeatBlocks but not in the second" in {
        val testData = Table(
          ("A", "B", "Expected Result"),
          (Seq((1, 10)), Seq((1, 10)), Seq.empty),
          (Seq((1, 3), (7, 9)), Seq((4, 6)), Seq((1, 3), (7, 9))),
          (Seq((1, 10)), Seq((5, 15)), Seq((1, 4))),
          (Seq((1, 10)), Seq((2, 4)), Seq((1, 1), (5, 10))),
          (Seq((1, 10)), Seq((4, 10)), Seq((1, 3))),
          (Seq((1, 10)), Seq((1, 11), (15, 20)), Seq.empty),
          (Seq((1, 10), (12, 20)), Seq((1, 10)), Seq((12, 20))),
          (Seq((1, 10), (12, 20)), Seq((12, 20)), Seq((1, 10))),
          (Seq((1, 10), (12, 20)), Seq((1, 10), (12, 20)), Seq.empty),
          (Seq((1, 5), (8, 9), (12, 20)), Seq((1, 20)), Seq.empty),
          (Seq((10, 18)), Seq((1, 9)), Seq((10, 18))),
          (Seq((10, 18)), Seq((1, 9), (15, 16)), Seq((10, 14), (17, 18))),
          (Seq((10, 18)), Seq((5, 12)), Seq((13, 18))),
          (Seq((1, 10)), Seq((5, 19)), Seq((1, 4))),
          (Seq((6, 6)), Seq((1, 20)), Seq.empty),
          (
            Seq((1, 10), (12, 20)),
            Seq((4, 5), (13, 19)),
            Seq((1, 3), (6, 10), (12, 12), (20, 20))
          )
        )

        forAll(testData) {
          (
              a: Seq[(Int, Int)],
              b: Seq[(Int, Int)],
              expectedResult: Seq[(Int, Int)]
          ) =>
            toSeatBlocks(a) -- toSeatBlocks(b) shouldBe toSeatBlocks(
              expectedResult
            )
        }
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
        SeatBlocks.of(ranges).seatCount shouldBe expectedSize
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
        an[IllegalArgumentException] should be thrownBy SeatBlocks.of(ranges)
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
        an[IllegalArgumentException] should be thrownBy SeatBlocks.of(ranges)
      }
    }
  }

  "The difference between SeatBlocks A and B" should {
    "be the ranges that is in A but not in B" in {
      val testData = Table(
        ("SeatBlocks A", "SeatBlocks B", "Diff"),
        (Seq((1, 20)), Seq((1, 10)), Seq((11, 20))),
        (Seq((5, 24)), Seq((24, 24)), Seq((5, 23))),
        (Seq((1, 20)), Seq((1, 20)), Seq.empty),
        (Seq((4, 8)), Seq((8, 8), (4, 5)), Seq((6, 7))),
        (
          Seq((1, 26)),
          Seq((1, 10), (12, 14), (21, 25)),
          Seq((11, 11), (15, 20), (26, 26))
        ),
        (
          Seq((1, 26)),
          Seq((5, 10), (12, 14), (21, 25)),
          Seq((1, 4), (11, 11), (15, 20), (26, 26))
        ),
        (Seq.empty, Seq((12, 15), (3, 6)), Seq.empty),
        (Seq((1, 20)), Seq.empty, Seq((1, 20))),
        (Seq((1, 20), (35, 40)), Seq.empty, Seq((1, 20), (35, 40)))
      )

      forAll(testData) {
        (
            first: Seq[(Int, Int)],
            second: Seq[(Int, Int)],
            diff: Seq[(Int, Int)]
        ) =>
          SeatBlocks.of(first) -- SeatBlocks.of(second) shouldBe SeatBlocks.of(
            diff
          )
      }
    }
  }

}
