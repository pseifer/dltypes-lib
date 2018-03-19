import de.uni_koblenz.dltypes.tools._
import org.scalatest._


class DLETest extends FreeSpec {
  "Simplifications works" in {
    val c = DLEConcept.simplify(Intersection(Union(Top, Bottom), Union(Bottom, Top)))
    assert(c == Top)
  }

  "The 'getVariables' method returns the set of all variables" in {
    val vs1 = Intersection(Union(Variable("?a"), Variable("?b")), Variable("?c")).getVariables
    assert(vs1 == Set(Variable("?a"), Variable("?b"), Variable("?c")))
    val vs2 = Intersection(Union(Variable("?a"), Variable("?b")), Variable("?b")).getVariables
    assert(vs2 == Set(Variable("?a"), Variable("?b")))
  }

  "The 'hasVariables' methods return true, if the concept contains variables" in {
    val vs1 = Intersection(Union(Variable("?a"), Variable("?b")), Variable("?c")).hasVariables
    assert(vs1)
    val vs2 = Intersection(Union(Concept(":A"), Concept(":B")), Concept(":C")).hasVariables
    assert(!vs2)
  }

  "Substitution works" in {
    val a = Intersection(Union(Variable("?a"), Variable("?b")), Variable("?a"))
    val s1 = DLEConcept.substitute(a, Variable("?a"), Top)
    assert(s1 == Intersection(Union(Top, Variable("?b")), Top))
    val s2 = DLEConcept.substitute(a, Variable("?c"), Top)
    assert(s2 == Intersection(Union(Variable("?a"), Variable("?b")), Variable("?a")))
  }
}
