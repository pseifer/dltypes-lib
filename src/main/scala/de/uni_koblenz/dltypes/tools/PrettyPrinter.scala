package de.uni_koblenz.dltypes
package tools


class PrettyPrinter {

  def dleRole(role: DLERole): String = {
    role match {
      case Role(iri) => iri.toString
      case Inverse(Role(iri)) => "¬" + iri
      case Data(iri) => iri.toString
    }
  }

  def dleConcept(dle: DLEConcept): String = {
    dle match {
      case Variable(_) => throw new Exception // TODO
      case Top => "⊤"
      case Bottom => "⊥"
      case Nominal(iri) => "{" + iri + "}"
      case Type(iri) => "\"\"^^<" + iri + ">"
      case Concept(iri) => iri.toString
      case Negation(expr) => "¬" + dleConcept(expr)
      case Intersection(lexpr, rexpr) => "(" + dleConcept(lexpr) + "⊓" + dleConcept(rexpr) + ")"
      case Union(lexpr, rexpr) => "(" + dleConcept(lexpr) + "⊔" + dleConcept(rexpr) + ")"
      case Existential(role, expr) => "∃" + dleRole(role) + "." + dleConcept(expr)
      case Universal(role, expr) => "∀" + dleRole(role) + "." + dleConcept(expr)
    }
  }
}
