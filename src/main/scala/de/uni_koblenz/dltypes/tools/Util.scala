package de.uni_koblenz.dltypes
package tools

object Util {
  val dlcharmap: Map[String, String] = Map(
    "$colon" -> ":",
    "$dot"   -> ".",
    "$hash"  -> "#" ,
    "$bar"   -> "|",
    "$amp"   -> "&",
    "$bang"  -> "!",
    "$less"  -> "<",
    "$greater"  -> ">",
    "$u007B" -> "{",
    "$u007D" -> "}",
    "$u0028" -> "(",
    "$u0029" -> ")",
    "$u0020" -> " ",
    "$u002E" -> ".",
    "$u22A4" -> "⊤",
    "$u22A5" -> "⊥",
    "$u2200" -> "∀",
    "$u2203" -> "∃",
    "$u00AC" -> "¬",
    "$u2293" -> "⊓",
    "$u2294" -> "⊔")

  // Decode the escape special symbols in quoted identifiers,
  // so they can be parsed.
  def decode(s: String): String =
    dlcharmap.foldLeft(s) { (a,b) => a.replaceAllLiterally(b._1, b._2) }

  // Test is the string contains any of the symbols
  // used to construct DL types.
  def isDLTypeHeuristic(s: String): Boolean =
    dlcharmap.keys.exists(s.contains)
}
