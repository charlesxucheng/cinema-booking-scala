package cinema

import org.scalatest.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait UnitSpec
    extends AnyWordSpec
    with Matchers
    with OptionValues
    with EitherValues
    with Inside
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with TableDrivenPropertyChecks
    with ScalaCheckPropertyChecks {
  val LS: String = System.lineSeparator()
}
