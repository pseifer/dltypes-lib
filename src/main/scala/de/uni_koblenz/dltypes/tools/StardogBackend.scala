package de.uni_koblenz.dltypes.tools

import collection.JavaConverters._

import com.complexible.stardog.api.ConnectionConfiguration
import de.uni_koblenz.dltypes.runtime.IRI


object StardogBackend {
  var URL = "http://localhost:5820"
  var database = "lubm"
  var user = "admin"
  var password = "admin"
  var ontology: String = "http://swat.cse.lehigh.edu/onto/univ-bench.owl#"

  def prefix: String = s"PREFIX : <$ontology>\n"

  val con = ConnectionConfiguration
    .to(database)
    .server(URL)
    .credentials(user, password)
    .reasoning(true)
    .connect()

  def ask(query: String): Boolean =
    try {
      return con.ask(prefix + query).execute()
    } catch {
      case e: Exception =>
        println(e)
        false
    }

  def run(query: String): List[List[IRI]] =
    try {
      val results = con.select(prefix + query).execute()
      val bindings = results.getBindingNames.asScala
      try {
        val lb = scala.collection.mutable.ListBuffer.empty[List[IRI]]
        while (results.hasNext) {
          val r = results.next()
          val li = scala.collection.mutable.ListBuffer.empty[IRI]
          for (b <- bindings)
            li += IRI(r.getValue(b).stringValue())
          lb += li.toList
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
