package de.uni_koblenz.dltypes
package tools


class PrettyPrinter {

  def dleQuery(q: DLEQuery): String = q match {
    case IndividualsForConcept(c) => "QUERY " + dleConcept(c)
    case IndividualsForRole(i, r) => "QUERY " + dleIndividual(i) + "." + dleRole(r)
  }

  def dleTruthy(t: DLETruthy): String = t match {
    case Subsumed(l, r) => dleConcept(l) + " ⊏ " + dleConcept(r)
    case IndividualEquality(a, b) => dleIndividual(a) + " = " + dleIndividual(b)
    case ConceptEquality(l, r) => dleConcept(l) + " ≡ " + dleConcept(r)
    case MemberOf(i, c) => dleIndividual(i) + " ∈ " + dleConcept(c)
    case Satisfiable(c) => dleConcept(c) + " ≢ ⊥"
    case Unsatisfiable(c) => dleConcept(c) + " ≡ ⊥"
  }

  def dleIndividual(i: DLEIndividual): String = i match {
    case Individual(in) => in
    case in @ IndividualN(_) => in.toString
  }

  def dleRole(role: DLERole): String = role match {
      case Role(iri) => iri.toString
      case Inverse(Role(iri)) => "¬" + iri
      case Data(iri) => iri.toString
  }

  def dleConcept(dle: DLEConcept): String = dle match {
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

  def print(dle: DLE): String = dle match {
    case t: DLETruthy => dleTruthy(t)
    case i: DLEIndividual => dleIndividual(i)
    case r: DLERole => dleRole(r)
    case c: DLEConcept => dleConcept(c)
    case q: DLEQuery => dleQuery(q)
  }
}
