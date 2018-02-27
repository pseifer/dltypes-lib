import de.uni_koblenz.dltypes.tools.StardogBackend
import org.scalatest._

class StardogTest extends FreeSpec {
  "simple example" in {
    val r = StardogBackend.run(
      """
        |PREFIX uni: <http://swat.cse.lehigh.edu/onto/univ-bench.owl#>
        |SELECT ?x WHERE { ?x a uni:Professor } LIMIT 10
      """.stripMargin)
    println(r)
  }

  "example" in {
    val r = StardogBackend.run(
      """
        |PREFIX uni: <http://swat.cse.lehigh.edu/onto/univ-bench.owl#>
        |
        |SELECT ?person
        |WHERE {
        |      ?person a [
        |        owl:intersectionOf (
        |          :Person
        |          [
        |              owl:onProperty :worksFor ;
        |              owl:someValuesFrom :Organization
        |          ]
        |        )
        |      ] .
        |} LIMIT 10
      """.stripMargin)
    println(r)
  }

  "ask example" in {
    val b = StardogBackend.ask(
      """
        |PREFIX uni: <http://swat.cse.lehigh.edu/onto/univ-bench.owl#>
        |ASK {
        |    <http://www.Department9.University0.edu/FullProfessor0> a uni:Professor
        |}
      """.stripMargin)
    println(b)
  }
}
