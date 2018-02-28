package de.uni_koblenz.dltypes
package tools


class PrettyPrinter {

  /* Pretty print DLEConcept
  --------------------------
  sealed trait DLEConcept extends DLE
  case object Top extends DLEConcept
  case object Bottom extends DLEConcept
  case class Nominal(iri: String) extends DLEConcept
  case class Concept(iri: String) extends DLEConcept
  case class Negation(expr: DLEConcept) extends DLEConcept
  case class Intersection(lexpr: DLEConcept, rexpr: DLEConcept) extends DLEConcept
  case class Union(lexpr: DLEConcept, rexpr: DLEConcept) extends DLEConcept
  case class Existential(role: DLERole, expr: DLEConcept) extends DLEConcept
  case class Universal(role: DLERole, expr: DLEConcept) extends DLEConcept
  * */

  def dleRole(role: DLERole): String = {
    role match {
      case Role(iri) => iri
      case Inverse(iri) => "¬" + iri
    }
  }

  def dleConcept(dle: DLEConcept): String = {
    dle match {
      case Top => "⊤"
      case Bottom => "⊥"
      case Nominal(iri) => "{" + iri + "}"
      case Concept(iri) => iri
      case Negation(expr) => "¬" + dleConcept(expr)
      case Intersection(lexpr, rexpr) => "(" + dleConcept(lexpr) + "⊓" + dleConcept(rexpr) + ")"
      case Union(lexpr, rexpr) => "(" + dleConcept(lexpr) + "⊔" + dleConcept(rexpr) + ")"
      case Existential(role, expr) => "∃" + dleRole(role) + "." + dleConcept(expr)
      case Universal(role, expr) => "∀" + dleRole(role) + "." + dleConcept(expr)
    }
  }
}
