import de.uni_koblenz.dltypes.tools._
import org.scalatest._

import scala.util.parsing.input.CharSequenceReader


class PrettyPrinterTest extends Parser with FlatSpecLike with Matchers {
  // Instantiate (default) pretty printer.
  val pp = new PrettyPrinter

  // Utility wrapper for expected results.
  private def roundtripping[T](s: String)(implicit p: Parser[T]): Boolean = {
    val phraseParser = phrase(p)
    val input = new CharSequenceReader(s)
    println(s)
    phraseParser(input).map { t =>
      println(t)
      val s = pp.print(t.asInstanceOf[DLEConcept])
      val input = new CharSequenceReader(s)
      println(s)
      phraseParser(input) match {
        case Success(t2, _) =>
          println(t2)
          t2 == t
        case NoSuccess(msg, _) => throw new IllegalArgumentException(
          "Parser error in '" + s + "': " + msg)
      }
    }.getOrElse(false)
  }

  "A pretty printed expression" should "parse as the same expression" in {
    implicit val parserToTest: Parser[DLEConcept] = dlexpr
    assert(roundtripping("⊤"))
    assert(roundtripping("⊥"))
    assert(roundtripping("{<:PeterMccoyChardonnay>}"))
    assert(roundtripping("<:Wine>"))
    assert(roundtripping("¬<:Wine>" ))
    assert(roundtripping("<:RedWine> ⊓ <:WhiteWine>"))
    assert(roundtripping("<:RedWine> ⊔ <:WhiteWine>"))
    assert(roundtripping("∃<:hasColor>.{<:Red>}"))
    assert(roundtripping("∀<:hasColor>.{<:Red>}"))
    assert(roundtripping("<:Wine> & <:Food> | <:Door> & <:Wall>"))
    assert(roundtripping("(<:Wine> & <:Food> | <:Door>) & <:Wall>"))
    assert(roundtripping("#A<:hasColor>.(#A<:type>.<:Wine>)"))
    assert(roundtripping("#A<:hasColor>.#A<:type>.<:Wine>"))
    assert(roundtripping("#A<:hasColor>.#E<:type>.<:Wine>"))
    assert(roundtripping("#A<:hasColor>.{<:Red>} | {<:White>}"))
    assert(roundtripping("#A<:hasColor>.{<:Red>} | {<:White>}"))
    assert(roundtripping("#A<:hasColor>.({<:Red>} | {<:White>})"))
  }

}
