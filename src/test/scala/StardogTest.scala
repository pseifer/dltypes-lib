import de.uni_koblenz.dltypes.tools.StardogBackend
import org.scalatest._

import de.uni_koblenz.dltypes.runtime.DLType


class StardogTest extends FreeSpec {

  "simple example" in {
    StardogBackend.connect("http://localhost:5820", "lubm")
    val r = StardogBackend.run(
      """
        |PREFIX uni: <http://swat.cse.lehigh.edu/onto/univ-bench.owl#>
        |SELECT ?x WHERE { ?x a uni:Professor }
      """.stripMargin, "1")

    val l = r.map(_.productElement(0)).asInstanceOf[List[Option[DLType]]]
    l.foreach { row: Option[DLType] =>
      println(row)
    }

    StardogBackend.disconnect()
  }

  "example" in {
    StardogBackend.connect("http://localhost:5820", "lubm")
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
      """.stripMargin, "0")
    println(r)
    StardogBackend.disconnect()
  }

  "ask example" in {
    StardogBackend.connect("http://localhost:5820", "lubm")
    val b = StardogBackend.ask(
      """
        |PREFIX uni: <http://swat.cse.lehigh.edu/onto/univ-bench.owl#>
        |ASK {
        |    <http://www.Department9.University0.edu/FullProfessor0> a uni:Professor
        |}
      """.stripMargin)
    println(b)
    StardogBackend.disconnect()
  }
}
