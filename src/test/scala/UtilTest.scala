import org.scalatest._
import de.uni_koblenz.dltypes.tools.Util._

class UtilTest extends FreeSpec {
  "encoding" in {
    assert(decode("$u2203$colonheadOf$u002E$u22A4") == "∃:headOf.⊤")
  }
}
