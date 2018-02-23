package de.uni_koblenz.dltypes
package tools

object Util {
  def simplify(concept: DLEConcept): DLEConcept = {
    def p(c: DLEConcept): DLEConcept = {
      c match {
        // Rewrite Universal to Negation.
        case Universal(r, expr) => Negation(Existential(r, Negation(expr)))
        // Simplify Bottom cases.
        case Negation(Bottom) => Top
        case Universal(_, Bottom) => Bottom // ?
        case Existential(_, Bottom) => Bottom
        case Union(Bottom, expr) => p(expr)
        case Union(expr, Bottom) => p(expr)
        case Intersection(Bottom, _) => Bottom
        case Intersection(_, Bottom) => Bottom
        // Simplify Top cases.
        case Negation(Top) => Bottom
        case Universal(_, Top) => Top // ?
        case Union(Top, _) => Top
        case Union(_, Top) => Top
        case Intersection(Top, expr) => p(expr)
        case Intersection(expr, Top) => p(expr)
        // Traverse.
        case Top => Top
        case Bottom => Bottom
        case Nominal(i) => Nominal(i)
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

  val dlcharmap: Map[String, String] = Map(
    "$colon" -> ":",
    "$dot"   -> ".",
    "$hash"  -> "#" ,
    "$bar"   -> "|",
    "$amp"   -> "&",
    "$bang"  -> "!",
    "$u002E" -> ".",
    "$u22A4" -> "⊤",
    "$u22A5" -> "⊥",
    "$u2200" -> "∀",
    "$u2203" -> "∃",
    "$u00AC" -> "¬",
    "$u2293" -> "⊓",
    "$u2294" -> "⊔")

  def decode(s: String): String =
    dlcharmap.foldLeft(s) { (a,b) => a.replaceAllLiterally(b._1, b._2) }

  def isDLTypeHeuristic(s: String): Boolean =
    dlcharmap.keys.exists(s.contains)
}
