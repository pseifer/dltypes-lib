package de.uni_koblenz.dltypes.tools


object QueryBuilder {
  private val parser = new Parser

  private val prefixes =
    """
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
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
    concept match {
      //case Top => ""
      //case Bottom => ""
      //case Nominal(i) => ""
      case Concept(i) => s"$s a $i."
      case Negation(c) =>
        val v = Gensym.fresh()
        s"$s a $v. FILTER NOT EXISTS { ${conceptToQuery(s, c)} }"
      //case Universal(Role(r), rexpr) => "TODO"
        // IRI(<>).isInstanceOf[#A :headOf.:Department]
        // -> !ASK { NOT EXISTS { <> :headOf ?x. MINUS { ?x a :Department. } } }
        // { <> :headOf ?x. NOT EXISTS { ?x a :Department. } } ?
      //case Universal(Role(r), expr) =>
      //  conceptToQuery() + s"NOT EXISTS { $s  }"
      case Existential(Role(r), Nominal(n)) => s"$s $r $n."
      case Existential(Role(r), Top) =>
        val v = Gensym.fresh()
        s"$s $r $v."
      case Existential(Role(r), expr) =>
        val v = Gensym.fresh()
        s"$s $r $v." + conceptToQuery(v, expr)
      // Union.
      case Union(lexpr, rexpr) => s"{ ${conceptToQuery(s, lexpr)} } UNION { ${conceptToQuery(s, rexpr)} }"
      // Intersection.
      case Intersection(lexpr, rexpr) => conceptToQuery(s, lexpr) + conceptToQuery(s, rexpr)
      case _ => throw new RuntimeException("[DL-RUNTIME] isInstanceOf not implemented for this case: " + concept)
    }
  }

  def askInstanceOf(value: String, tpe: String): String = {
    val dle = parser.parse(parser.dlexpr, Util.decode(tpe)) match {
      case parser.Success(m, _) => m.asInstanceOf[DLEConcept]
      case parser.NoSuccess(s, msg) => throw new RuntimeException("[DL-RUNTIME] invalid type." + msg + " " + s)
      // TODO Note: This has no business being <actually> detected during runtime!
    }

    Gensym.reset()
    prefixes + s"ASK { ${conceptToQuery(value, Util.simplify(dle))} }"
  }
}


