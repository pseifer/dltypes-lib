package de.uni_koblenz.dltypes
package tools

object Util {
  val dlcharmap: List[(String, String)] =
    ("$colon", ":") ::
    ("$hash",  "#") ::
    ("$bar",   "|") ::
    ("$amp",   "&") ::
    ("$bang",  "!") ::
    ("$u22A4", "⊤") ::
    ("$u22A5", "⊥") ::
    ("$u2200", "∀") ::
    ("$u2203", "∃") ::
    ("$u00AC", "¬") ::
    ("$u2293", "⊓") ::
    ("$u2294", "⊔") ::
    Nil

  val dlchars: List[String] = dlcharmap.map(_._1)

  def isDLTypeHeuristic(s: String): Boolean =
    dlchars.exists(s.contains)
}
