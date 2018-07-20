package de.uni_koblenz.dltypes
package runtime

import tools._

// The static annotation representing DL types (internally).
case class dl(s: String) extends scala.annotation.StaticAnnotation

// Type that is used to represent DLTypes at compile time
// and is also the supertype of all runtime DLTypes.
sealed trait DLType {
  def isSubsumed(tpe: DLEConcept, prefixes: String): Boolean

  def string: String
  def int: Int
  def float: Float
  def double: Double
  def boolean: Boolean
}

// Implementation of DLType.
case class IRI(value: String) extends DLType {
  def isSubsumed(tpe: DLEConcept, prefixes: String): Boolean = {
    val simple = DLEConcept.simplifyPlus(tpe)
    simple match {
      case Top => true
      case Bottom => false
      case _ =>
        StardogBackend.ask(QueryBuilder.askInstanceOf(this.toString, simple, prefixes))
    }
  }

  def string: String = value
  def int: Int = value.toInt
  def float: Float = value.toFloat
  def double: Double = value.toDouble
  def boolean: Boolean = value.toBoolean

  override def toString = "<" + value + ">"
}

// StringBuilder and connection API for SPARQL.
object Sparql {

  // Connect to triple store.
  def connect(url: String, db: String): Unit =
    StardogBackend.connect(url, db)

  // Connect to triple store.
  def connect(url: String, db: String, user: String, pwd: String): Unit =
    StardogBackend.connect(url, db, user, pwd)

  // Disconnect from triple store.
  def disconnect(): Unit =
    StardogBackend.disconnect()

  // Clean up a value to avoid injections.
  private def clean(value: String) =
    value.filter(_ != '"')

  // Wrap a value as SPARQL data value.
  private def wrap(value: String, typename: String) = "\"" + value + "\"" + "^^" + typename

  // Sparql queries with only one argument do not return List of Product, but a simple List[IRI]
  implicit class Sparql1Helper(val sc: StringContext) extends AnyVal {
    def sparql1(args: Any*): List[Any] = {
      val r = SparqlHelper(sc).sparql(args: _*)
      // Unwrap Tuple1
      r.map(_.productElement(0))
    }

    def strictsparql1(args: Any*): List[Any] = sparql1(args: _*)
  }

  implicit class SparqlHelper(val sc: StringContext) extends AnyVal {
    def sparql(args: Any*): List[Product] = {
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
          case i: DLType => clean(i.toString)
          // This could only happen if compilation failed to
          // detect another case.
          case t => throw new RuntimeException(
            s"""
              |[DL] Type $t can not be inserted.
              |This exception should never occur at runtime.
            """.stripMargin)
        }
        buf.append(e)
        buf.append(strings.next)
      }

      val s = buf.toString
      val hint = s.takeWhile(_ != '|')
      val query = s.dropWhile(_ != '|').tail
      StardogBackend.run(query, hint)
    }

    // Strict type checking for result type.
    def strictsparql(args: Any*): List[Product] = sparql(args)
  }

  implicit class IriHelper(val sc: StringContext) extends AnyVal {
    def iri(args: Any*): IRI = {
      val strings = sc.parts.iterator
      val s: String = strings.next
      IRI(s)
    }
  }
}
