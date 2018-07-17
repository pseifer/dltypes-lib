package de.uni_koblenz.dltypes
package tools

import scala.util.parsing.combinator._


class Parser extends RegexParsers {
  def dlexpr: Parser[DLEConcept] = union
  def union: Parser[DLEConcept] = rep1sep(inter, UNION_TOKEN) ^^ { _.reduceLeft(Union) }
  def inter: Parser[DLEConcept] = rep1sep(f, INTER_TOKEN) ^^ { _.reduceLeft(Intersection) }
  def f: Parser[DLEConcept] =
    ( UNIVERSAL_TOKEN ~ data ~ "." ~ stype ^^ { case _ ~ r ~ _ ~ t => Universal(r, t) }
    | EXISTENTIAL_TOKEN ~ data ~ "." ~ stype ^^ { case _ ~ r ~ _ ~ t => Existential(r, t) }
    | UNIVERSAL_TOKEN ~ role ~ "." ~ f ^^ { case _ ~ r ~ _ ~ c => Universal(r, c) }
    | EXISTENTIAL_TOKEN ~ role ~ "." ~ f ^^ { case _ ~ r ~ _ ~ c => Existential(r, c) }
    | concept
    | NEGATION_TOKEN ~ f ^^ { case _ ~ f => Negation(f) }
    | "(" ~ dlexpr ~ ")" ^^ { case _ ~ e ~ _ => e }
    )
  def concept: Parser[DLEConcept] = TOP | BOTTOM | NOMINAL | CONCEPT
  def stype: Parser[DLEConcept] = SCALA_TYPE //| OTHER_TYPE
  def role: Parser[DLERole] = ROLE | NEGATED_ROLE
  def data: Parser[DLERole] = IRI ^^ { r => Data(r) }

  // Utility parser for specific concepts and roles.
  def IRI: Parser[String] = SIMPLE_IRI | DELIM_IRI
  def SIMPLE_IRI: Parser[String]  = """[:a-zA-Z0-9]+""".r ^^ { s => s"$s" }
  def DELIM_IRI: Parser[String]  = "<" ~ """[^>]+""".r ~ ">" ^^ { case _ ~ s ~ _ => s"${s.toString}" }
  def TOP: Parser[Top.type] = TOP_TOKEN ^^ { _ => Top }
  def BOTTOM: Parser[Bottom.type] = BOTTOM_TOKEN ^^ { _ => Bottom }
  def NOMINAL: Parser[Nominal] = "{" ~ IRI ~ "}" ^^ { case _ ~ s ~ _ => Nominal(s) }
  def CONCEPT: Parser[Concept] = IRI ^^ { s => Concept(s) }
  def ROLE: Parser[Role] = IRI ^^ { r => Role(r) }
  def NEGATED_ROLE: Parser[Inverse] = NEGATION_TOKEN ~ ROLE ^^ { case _ ~ r => Inverse(r) }
  def INDIVIDUAL: Parser[DLEIndividual] = IRI ^^ { case i => Individual(i) }
  //def OTHER_TYPE: Parser[DLEConcept] = "<" ~ IRI ~ ">" ^^ { case _ ~ t ~ _ => Type(t.toString) }
  def SCALA_TYPE: Parser[DLEConcept] = SCALA_TYPES ^^ { t => Type(t.toString) }
  def SCALA_TYPES: Parser[Any] = INT | FLOAT | DOUBLE | BOOLEAN | STRING

  // Tokens.
  def UNION_TOKEN: Parser[Any] = "|" | "⊔"
  def INTER_TOKEN: Parser[Any] = "&" | "⊓"
  def UNIVERSAL_TOKEN: Parser[Any] = "#A" | "∀"
  def EXISTENTIAL_TOKEN: Parser[Any] = "#E" | "∃"
  def NEGATION_TOKEN: Parser[Any] = "!" | "¬"
  def TOP_TOKEN: Parser[Any] = "#t" | "⊤"
  def BOTTOM_TOKEN: Parser[Any] = "#f" | "⊥"

  def INT: Parser[Any] = "Int" ^^ { _ => "Int" }
  def FLOAT: Parser[Any] = "Float" ^^ { _ => "Float" }
  def DOUBLE: Parser[Any] = "Double" ^^ { _ => "Double" }
  def BOOLEAN: Parser[Any] = "Boolean" ^^ { _ => "Boolean" }
  def STRING: Parser[Any] = "String" ^^ { _ => "String" }
}
