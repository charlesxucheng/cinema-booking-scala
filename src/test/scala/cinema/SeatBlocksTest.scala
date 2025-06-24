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

}
