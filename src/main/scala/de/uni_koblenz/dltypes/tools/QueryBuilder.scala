package de.uni_koblenz.dltypes.tools


object QueryBuilder {
  private val parser = new Parser

  private def prefixes(globals: String) =
    s"""
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
      |$globals
    """.stripMargin

  private object Gensym {
    private var c: Int = 0
    def fresh(): String = {
      c += 1
      s"?$c"
    }
    def reset(): Unit = c = 0
  }

  def conceptToQuery(s: String, concept: DLEConcept): String = {
    // The concept can not be Top or Bottom, these
    // cases are filtered out in the isSubsumed method.
    // Due to the usage of simplifyPlus, query never contains Universal.
    concept match {
      case Concept(i) => s"$s a $i."
      case Negation(c) =>
        val v = Gensym.fresh()
        s"$s a $v. FILTER NOT EXISTS { ${conceptToQuery(s, c)} }"
      case Existential(Role(r), Nominal(n)) => s"$s $r $n."
      case Existential(Role(r), Top) =>
        val v = Gensym.fresh()
        s"$s $r $v."
      case Existential(Role(r), expr) =>
        val v = Gensym.fresh()
        s"$s $r $v." + conceptToQuery(v, expr)
      case Union(lexpr, rexpr) => s"{ ${conceptToQuery(s, lexpr)} } UNION { ${conceptToQuery(s, rexpr)} }"
      case Intersection(lexpr, rexpr) => conceptToQuery(s, lexpr) + conceptToQuery(s, rexpr)
      case _ => throw new RuntimeException("[DL-RUNTIME] isInstanceOf not implemented for this case: " + concept)
    }
  }

  def askInstanceOf(value: String, tpe: DLEConcept, globalPrefixes: String): String = {
    Gensym.reset()
    prefixes(globalPrefixes) + s"ASK { ${conceptToQuery(value, tpe)} }"
  }
}
