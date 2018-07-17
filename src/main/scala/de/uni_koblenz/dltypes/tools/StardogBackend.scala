package de.uni_koblenz.dltypes.tools

import collection.JavaConverters._
import com.complexible.stardog.api.{Connection, ConnectionConfiguration}
import de.uni_koblenz.dltypes.runtime.IRI


object StardogBackend {
  var con: Option[Connection] = None

  def connect(url: String, db: String): Unit = {
    disconnect()
    con = Some(ConnectionConfiguration
      .to(db)
      .server(url)
      .reasoning(true)
      .connect())
  }

  def connect(url: String, db: String, user: String, pwd: String): Unit = {
    disconnect()
    con = Some(ConnectionConfiguration
      .to(db)
      .server(url)
      .credentials(user, pwd)
      .reasoning(true)
      .connect())
  }

  def disconnect(): Unit = {
    if (con.isDefined && con.get.isOpen())
      con.get.close()
  }

  def ask(query: String): Boolean =
    try {
      return con.get.ask(query).execute()
    } catch {
      case e: Exception =>
        println(e)
        false
    }

  def run(query: String, typeHint: String): List[Product] = {
    val hints = typeHint.map( x =>
      if (x == '0') false
      else true)

    try {
      val results = con.get.select(query).execute()
      val bindings = results.getBindingNames.asScala
      val bindingsAndHints = bindings.zip(hints)
      try {
        val lb = scala.collection.mutable.ListBuffer.empty[Product]
        while (results.hasNext) {
          val r = results.next()
          val li = scala.collection.mutable.ListBuffer.empty[Any]
          for ((b, isOpt) <- bindingsAndHints) {
            val temp = r.getValue(b)

            if (isOpt) {
              if (temp == null)
                li += (None : Option[IRI])
              else
                li += (Some(IRI(temp.stringValue())) : Option[IRI])
            }
            else {
              if (temp == null)
                li += IRI("")
              else
                li += IRI(temp.stringValue())
            }
          }
          val l = li.length
          try {
            lb += {
              l match {
                case 1 => Tuple1(li(0))
                case 2 => Tuple2(li(0), li(1))
                case 3 => Tuple3(li(0), li(1), li(2))
                case 4 => Tuple4(li(0), li(1), li(2), li(3))
                case 5 => Tuple5(li(0), li(1), li(2), li(3), li(4))
                case 6 => Tuple6(li(0), li(1), li(2), li(3), li(4), li(5))
                case 7 => Tuple7(li(0), li(1), li(2), li(3), li(4), li(5), li(6))
                case 8 => Tuple8(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7))
                case 9 => Tuple9(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8))
                case 10 => Tuple10(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9))
                case 11 => Tuple11(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10))
                case 12 => Tuple12(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11))
                case 13 => Tuple13(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12))
                case 14 => Tuple14(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13))
                case 15 => Tuple15(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13), li(14))
                case 16 => Tuple16(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13), li(14), li(15))
                case 17 => Tuple17(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13), li(14), li(15), li(16))
                case 18 => Tuple18(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13), li(14), li(15), li(16), li(17))
                case 19 => Tuple19(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13), li(14), li(15), li(16), li(17), li(18))
                case 20 => Tuple20(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13), li(14), li(15), li(16), li(17), li(18), li(19))
                case 21 => Tuple21(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13), li(14), li(15), li(16), li(17), li(18), li(19), li(20))
                case 22 => Tuple22(li(0), li(1), li(2), li(3), li(4), li(5), li(6), li(7), li(8), li(9), li(10), li(11), li(12), li(13), li(14), li(15), li(16), li(17), li(18), li(19), li(20), li(21))
                case _ => throw new RuntimeException("Query has more than 22 variables. Should have been caught at compile time.")
              }
            }
          } catch {
            case _: IndexOutOfBoundsException => throw new RuntimeException("Internal query result has wrong dimension. Should have been caught at compile time.")
          }
        }
        lb.toList
      } finally {
        results.close()
      }
    }
    catch {
      case e: Exception => println(e)
        Nil
    }
  }
}
