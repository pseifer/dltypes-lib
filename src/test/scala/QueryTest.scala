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

  // Negation test:

  "test00" in {
    val x = StardogBackend.ask(
      """
        |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |PREFIX owl: <http://www.w3.org/2002/07/owl#>
        |ASK {
        |  <http://www.Department0.University0.edu/FullProfessor0> a ?y.
        |  FILTER NOT EXISTS {
        |    { <http://www.Department0.University0.edu/FullProfessor0> a :Student }
        |    UNION
        |    { <http://www.Department0.University0.edu/FullProfessor0> a :FullProfessor }
        |  }.
        |}
      """.stripMargin)
    assert(!x)
  }

  "negation no" in {
    val iri = IRI("<http://www.Department0.University0.edu/FullProfessor0>")
    assert(!iri.isSubsumed("!(:Student ⊔ :FullProfessor)"))
  }

  "negation yes" in {
    val iri = IRI("<http://www.Department0.University0.edu/FullProfessor0>")
    assert(iri.isSubsumed("!(:Student ⊔ :GraduateStudent)"))
  }

  // Forall test:

  // #E :headOf.T
  // => ?x a ?y. ?y :headOf []. // there is any old y, for which this is true

  // #A :headOf.
  // => ?x a ?y. ?y  // should be: for all ?y, there is :headOf
  //"forall" in {
  //  val x = JRDFoxBackend.run(
  //    """
  //      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  //      |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  //      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
  //      |
  //      |SELECT ?x WHERE {
  //      |  ?x a ?y.
  //      |  FILTER NOT EXISTS {
  //      |    ?x :headOf ?z.
  //      |    ?z a ?a.
  //      |    FILTER NOT EXISTS {
  //      |      ?a a :Department.
  //      |    }
  //      |  }
  //      |}
  //    """.stripMargin)
  //  println(x)
  //  assert(true)
  //}

  "univ" in {
    val prefixes =
    """
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
    """.stripMargin

    val dl = Existential(Role(":headOf"), Negation(Concept(":Department")))
    println(dl)
    val ndl = Util.simplify(dl)
    println(ndl)
    val query = QueryBuilder.conceptToQuery("<http://www.Department0.University0.edu/FullProfessor0>", ndl)
    println(query)
    val x = StardogBackend.ask(prefixes + s"ASK { $query }")
    println(x)
    assert(true)
  }
}
