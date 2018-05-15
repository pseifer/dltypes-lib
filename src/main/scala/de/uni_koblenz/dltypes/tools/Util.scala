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
    "$u007B" -> "{",
    "$u007D" -> "}",
    "$u0020" -> " ",
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
