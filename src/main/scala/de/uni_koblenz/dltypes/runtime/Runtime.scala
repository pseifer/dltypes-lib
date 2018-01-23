package de.uni_koblenz.dltypes
package runtime

// Type that is used to represent DLTypes at compile time
// and is also the supertype of all runtime DLTypes.
sealed trait DLType

// Simple IRI value.
case class IRI(value: String) extends DLType

object Sparql {
  // This class builds and executes queries, but can be part of the library code.
  // This also allows for the possibility to provide some utility and a
  // default implementation while allowing for easy adaptation to different
  // SPARQL libraries / back ends in user code OR library code.
  implicit class SparqlHelper(val sc: StringContext) extends AnyVal {
    def sparql(args: Any*): List[IRI] = {
      // This part as some utility (in a base class) and in the
      // DL runtime classes (toString, etc.).
      val strings = sc.parts.iterator
      val exprs = args.iterator
      var buf = new StringBuffer(strings.next)
      while(strings.hasNext) { // exprs.length == strings.length + 1 is always true
        buf.append(exprs.next)
        buf.append(strings.next)
      }
      // TODO: Execute constructed query.
      println("+*+ Executing SPARQL Query +*+")
      println(buf.toString)
      List(IRI("RedWine1"), IRI("RedWine2")) // Query "result".
    }
  }

  implicit class IriHelper(val sc: StringContext) extends AnyVal {
    def iri(args: Any*): IRI = {
      val strings = sc.parts.iterator
      val s: String = strings.next
      IRI(s)
    }
  }
}
