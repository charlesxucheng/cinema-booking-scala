package cinema

import org.scalatest.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

trait UnitSpec extends AnyWordSpec with Matchers with OptionValues with EitherValues with Inside with BeforeAndAfterAll with BeforeAndAfterEach with TableDrivenPropertyChecks {
  val LS: String = System.lineSeparator()
}

