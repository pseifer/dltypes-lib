package de.uni_koblenz.dltypes
package runtime

import tools._


// Type that is used to represent DLTypes at compile time
// and is also the supertype of all runtime DLTypes.
sealed trait DLType {
  def isSubsumed(tpe: DLEConcept): Boolean
}


case class IRI(value: String) extends DLType {
  def isSubsumed(tpe: DLEConcept): Boolean =
    StardogBackend.ask(QueryBuilder.askInstanceOf(value, tpe))
}


object Sparql {
  implicit class SparqlHelper(val sc: StringContext) extends AnyVal {
    def sparql(args: Any*): List[List[IRI]] = {
      val strings = sc.parts.iterator
      val exprs = args.iterator
      val buf = new StringBuffer(strings.next)
      while(strings.hasNext) { // exprs.length == strings.length + 1 is always true
        buf.append(exprs.next)
        buf.append(strings.next)
      }
      StardogBackend.run(buf.toString)
    }

    // Strict type checking for result type.
    def strictsparql(args: Any*): List[List[IRI]] = sparql(args)
  }


  implicit class IriHelper(val sc: StringContext) extends AnyVal {
    def iri(args: Any*): IRI = {
      val strings = sc.parts.iterator
      val s: String = strings.next
      IRI(s)
    }
  }
}
