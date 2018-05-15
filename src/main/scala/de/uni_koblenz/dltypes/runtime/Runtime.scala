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

  override def toString = value
}


object Sparql {

  // Clean up a value to avoid injections.
  private def clean(value: String) = {
    // TODO: Check for all fishy unicode chars.
    value.filter(_ != '"')
  }

  // Wrap a value as SPARQL data value.
  private def wrap(value: String, typename: String) = "\"" + value + "^^" + typename

  implicit class SparqlHelper(val sc: StringContext) extends AnyVal {
    def sparql(args: Any*): List[List[IRI]] = {
      val strings = sc.parts.iterator
      val exprs = args.iterator
      val buf = new StringBuffer(strings.next)

      while(strings.hasNext) { // exprs.length == strings.length + 1 is always true
        val e: String = exprs.next match {
          // XSD type mappings.
          case i: Int => wrap(i.toString, "xsd:integer")
          case f: Float => wrap(f.toString, "xsd:float")
          case d: Double => wrap(d.toString, "xsd:double")
          case s: String => wrap(clean(s), "xsd:string")
          case b: Boolean => wrap(b.toString, "xsd:boolean")
          // IRI
          case i: IRI => clean(i.toString)
          // This could only happen if compilation failed to
          // detect another case.
          case _ => throw new RuntimeException(
            """
              |[DL] This type can not be inserted.
              |This exception should never occur at runtime.
            """.stripMargin)
        }
        buf.append(e)
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
