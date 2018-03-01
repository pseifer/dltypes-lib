import de.uni_koblenz.dltypes.runtime.IRI
import de.uni_koblenz.dltypes.tools.{JRDFoxBackend, QueryBuilder}
import de.uni_koblenz.dltypes.tools._
import org.scalatest._


class QueryTest extends FreeSpec {

  "role concept" in {
    val prof = IRI("<http://www.Department9.University0.edu/FullProfessor0>")
    assert(prof.isSubsumed("∃:headOf.:Department"))
  }

  "role top" in {
    val prof = IRI("<http://www.Department9.University0.edu/FullProfessor0>")
    assert(prof.isSubsumed("∃:headOf.⊤"))
  }

  //"role nominal" in {
  //  val prof = IRI("<http://www.Department9.University0.edu/FullProfessor0>")
  //  assert(prof.isSubsumed("∃:headOf.{<http://www.Department9.University0.edu>}")) // IRI parser not done
  //}

  "intersection" in {
    val prof = IRI("<http://www.Department9.University0.edu/FullProfessor0>")
    assert(prof.isSubsumed(":Person ⊓ ∃:headOf.⊤"))
  }

  "union" in {
    val prof = IRI("<http://www.Department9.University0.edu/FullProfessor0>")
    assert(prof.isSubsumed(":Student ⊔ ∃:headOf.⊤"))
  }

  "concept success" in {
    val iri = IRI("<http://www.Department0.University0.edu/FullProfessor0>")
    assert(iri.isSubsumed(":Person"))
  }

  "concept failure" in {
    val iri = IRI("<http://www.Department0.University0.edu/FullProfessor0>")
    assert(!iri.isSubsumed(":Student"))
  }

  "ask1" in {
    val x = StardogBackend.ask(
      """
        |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |PREFIX owl: <http://www.w3.org/2002/07/owl#>

        |ASK { <http://www.Department0.University0.edu/FullProfessor0> a :FullProfessor }
      """.stripMargin)
    assert(x)
  }

  "ask2" in {
    val x = StardogBackend.ask(
      """
        |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |PREFIX owl: <http://www.w3.org/2002/07/owl#>

        |ASK { <http://www.Department0.University0.edu/FullProfessor0> a :Student }
      """.stripMargin)
    assert(!x)
  }

  // Negation

  "negation yes" in {
    val iri = IRI("<http://www.Department0.University0.edu/FullProfessor0>")
    assert(iri.isSubsumed("!(:Student ⊔ :GraduateStudent)"))
  }

  // Universal

  "universal +" in {
    val iri = IRI("<http://www.Department9.University0.edu/FullProfessor0>")
    assert(iri.isSubsumed("∀:headOf.:Department"))
  }
}
