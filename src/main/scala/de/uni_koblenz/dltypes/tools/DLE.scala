package de.uni_koblenz.dltypes
package tools

import scala.collection.mutable.SetBuilder
import org.semanticweb.owlapi.model._

object DLE {
  val pp = new PrettyPrinter
  val parser = new Parser

  def parse(tpt: String): Either[String, DLEConcept] =
    DLE.parser.parse(DLE.parser.dlexpr, Util.decode(tpt)) match {
      case DLE.parser.Success(m, _) =>
        Right(m.asInstanceOf[DLEConcept])
      case DLE.parser.NoSuccess(reason, _) =>
        Left(reason)
    }
}

sealed trait DLE {
  def pretty(stripPrefix: Option[String] = None): String = DLE.pp.print(this, stripPrefix)
}

sealed trait DLETruthy extends DLE
case class Subsumed(c: DLEConcept, d: DLEConcept) extends DLETruthy
case class IndividualEquality(a: DLEIndividual, b: DLEIndividual) extends DLETruthy
case class ConceptEquality(c: DLEConcept, d: DLEConcept) extends DLETruthy
case class MemberOf(a: DLEIndividual, c: DLEConcept) extends DLETruthy
case class Satisfiable(c: DLEConcept) extends DLETruthy
case class Unsatisfiable(c: DLEConcept) extends DLETruthy

sealed trait DLEQuery extends DLE
case class IndividualsForConcept(c: DLEConcept) extends DLEQuery
case class IndividualsForRole(a: DLEIndividual, r: DLERole) extends DLEQuery

sealed trait DLERole extends DLE
case class Role(r: String) extends DLERole
case class Inverse(role: Role) extends DLERole
case class Data(r: String) extends DLERole

sealed trait DLEIndividual extends DLE {
  def name: String
  def iri: String = name
}
case class Individual(i: String) extends DLEIndividual {
  def name: String = i
}

case class IndividualN(i: OWLNamedIndividual) extends DLEIndividual {
  def name: String = i.getIRI.getFragment
  override def iri: String = i.getIRI.toString
  override def toString: String = iri
}

sealed trait DLEConcept extends DLE {
  // Equality check that considers commutativity of intersection/union.
  def same(other: DLEConcept): Boolean = this == other

  // Returns the set of all variables that occur in the DLEConcept.
  def getVariables: Set[Variable] = {
    val variables = new SetBuilder[Variable, Set[Variable]](Set())
    def find(c: DLEConcept): Unit = { c match {
      case v @ Variable(_) => variables += v
      case Negation(expr) => find(expr)
      case Universal(_, expr) => find(expr)
      case Existential(_, expr) => find(expr)
      case Union(lexpr, rexpr) =>
        find(lexpr)
        find(rexpr)
      case Intersection(lexpr, rexpr) =>
        find(lexpr)
        find(rexpr)
      // Nothing to do for trivial cases.
      case _ => Unit
    } }
    // Recursively find variables in concept.
    find(this)
    variables.result()
  }

  // Returns true, if the DLEConcept contains any variables.
  def hasVariables: Boolean = {
    def find(c: DLEConcept): Boolean = { c match {
      case Variable(_) => true
      case Negation(expr) => find(expr)
      case Universal(_, expr) => find(expr)
      case Existential(_, expr) => find(expr)
      case Union(lexpr, rexpr) => find(lexpr) || find(rexpr)
      case Intersection(lexpr, rexpr) => find(lexpr) || find(rexpr)
      case _ => false
    } }
    find(this)
  }
}

case class Variable(v: String) extends DLEConcept
case class Type(t: String) extends DLEConcept
case object Top extends DLEConcept
case object Bottom extends DLEConcept
case class Nominal(n: String) extends DLEConcept
case class Concept(c: String) extends DLEConcept
case class Existential(role: DLERole, expr: DLEConcept) extends DLEConcept
case class Universal(role: DLERole, expr: DLEConcept) extends DLEConcept
case class Negation(expr: DLEConcept) extends DLEConcept

case class Intersection(lexpr: DLEConcept, rexpr: DLEConcept) extends DLEConcept {
  override def same(o: DLEConcept): Boolean = { o match {
    case Intersection(lo, ro) =>
      ((lo same this.lexpr) && (ro same this.rexpr)) ||
      ((ro same this.lexpr) && (lo same this.rexpr))
    case _ => false
  } }
}

case class Union(lexpr: DLEConcept, rexpr: DLEConcept) extends DLEConcept {
  override def same(o: DLEConcept): Boolean = { o match {
    case Union(lo, ro) =>
      ((lo same this.lexpr) && (ro same this.rexpr)) ||
      ((ro same this.lexpr) && (lo same this.rexpr))
    case _ => false
  } }
}

object DLEConcept {

  def intersectionOf(cs: List[DLEConcept]): DLEConcept =
    simplify(cs.foldLeft(Top: DLEConcept)(Intersection))

  def unionOf(cs: List[DLEConcept]): DLEConcept =
    simplify(cs.foldLeft(Bottom: DLEConcept)(Union))

  // Substitute variable with expression.
  def substitute(concept: DLEConcept, v: Variable, other: DLEConcept): DLEConcept = {
    def p(c: DLEConcept): DLEConcept = {
      c match {
        // Subst
        case vi@Variable(_) if v == vi => other
        case Variable(v) => Variable(v)
        // Traverse
        case Top => Top
        case Bottom => Bottom
        case Nominal(i) => Nominal(i)
        case Type(i) => Type(i)
        case Concept(i) => Concept(i)
        case Negation(c) => Negation(p(c))
        case Existential(r, expr) => Existential(r, p(expr))
        case Universal(r, expr) => Universal(r, p(expr))
        case Union(lexpr, rexpr) => Union(p(lexpr), p(rexpr))
        case Intersection(lexpr, rexpr) => Intersection(p(lexpr), p(rexpr))
      }
    }
    p(concept)
  }

  // Simplification rules for DLEs.
  def simplify(concept: DLEConcept): DLEConcept = {
    def p(c: DLEConcept): DLEConcept = {
      c match {
        case Union(expr1, expr2) if expr1 same expr2 => p(expr1)
        case Intersection(expr1, expr2) if expr1 same expr2 => p(expr1)
        case Intersection(expr1, Negation(expr2)) if expr1 same expr2 => Bottom
        case Intersection(Negation(expr1), expr2) if expr1 same expr2 => Bottom
        case Union(expr1, Negation(expr2)) if expr1 same expr2 => Top
        case Union(Negation(expr1), expr2) if expr1 same expr2 => Top
        case Negation(Negation(expr)) => p(expr)
        // a ^ (a v b) = a
        case Intersection(expr1, Union(expr2, _)) if expr1 same expr2 => p(expr1)
        case Intersection(expr1, Union(_, expr2)) if expr1 same expr2 => p(expr1)
        case Intersection(Union(expr2, _), expr1) if expr1 same expr2 => p(expr1)
        case Intersection(Union(_, expr2), expr1) if expr1 same expr2 => p(expr1)
        // a v (a ^ b) = a
        case Union(expr1, Intersection(expr2, _)) if expr1 same expr2 => p(expr1)
        case Union(expr1, Intersection(_, expr2)) if expr1 same expr2 => p(expr1)
        case Union(Intersection(expr2, _), expr1) if expr1 same expr2 => p(expr1)
        case Union(Intersection(_, expr2), expr1) if expr1 same expr2 => p(expr1)
        // Simplify Bottom cases
        case Negation(Bottom) => Top
        case Union(Bottom, expr) => p(expr)
        case Union(expr, Bottom) => p(expr)
        case Intersection(Bottom, _) => Bottom
        case Intersection(_, Bottom) => Bottom
        // Simplify Top cases
        case Negation(Top) => Bottom
        case Union(_, Top) => Top
        case Union(Top, _) => Top
        case Intersection(Top, expr) => p(expr)
        case Intersection(expr, Top) => p(expr)
        // Traverse
        case Top => Top
        case Bottom => Bottom
        case Nominal(i) => Nominal(i)
        case Type(i) => Type(i)
        case Concept(i) => Concept(i)
        case Negation(c) => Negation(p(c))
        case Existential(r, expr) => Existential(r, p(expr))
        case Universal(r, expr) => Universal(r, p(expr))
        case Union(lexpr, rexpr) => Union(p(lexpr), p(rexpr))
        case Intersection(lexpr, rexpr) => Intersection(p(lexpr), p(rexpr))
        case Variable(v) => Variable(v)
      }
    }
    p(concept)
  }

  def fullySimplify(concept: DLEConcept): DLEConcept = {
    val s = simplify(concept)
    if (s == concept)
      s
    else fullySimplify(s)
  }

  // Simplifications used for runtime check query generation.
  def simplifyPlus(concept: DLEConcept): DLEConcept = {
    def p(c: DLEConcept): DLEConcept = {
      c match {
        // Complex Top/Bottom rules
        case Universal(_, Bottom) => Bottom // ?
        case Existential(_, Bottom) => Bottom
        case Universal(_, Top) => Top // ?
        // Rewrite Universal to Negation
        case Universal(r, expr) => Negation(Existential(r, Negation(expr)))
        // Traverse
        case Top => Top
        case Bottom => Bottom
        case Nominal(i) => Nominal(i)
        case Type(i) => Type(i)
        case Concept(i) => Concept(i)
        case Variable(v) => Variable(v)
        case Negation(c) => Negation(p(c))
        case Existential(r, expr) => Existential(r, p(expr))
        case Union(lexpr, rexpr) => Union(p(lexpr), p(rexpr))
        case Intersection(lexpr, rexpr) => Intersection(p(lexpr), p(rexpr))
      }
    }
    p(simplify(concept))
  }
}

