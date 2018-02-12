package de.uni_koblenz.dltypes.tools

import java.io.File

import de.uni_koblenz.dltypes.runtime.IRI
import org.semanticweb.owlapi.apibinding.OWLManager
import uk.ac.ox.cs.JRDFox.Prefixes
import uk.ac.ox.cs.JRDFox.store.DataStore


object JRDFoxBackend {
  // TODO: Figure out how to best set this.
  val ontologySource: String = "http://swat.cse.lehigh.edu/onto/univ-bench.owl#"

  val manager = OWLManager.createOWLOntologyManager
  //val owlres = new File("ontology.owl")
  //val ontology = manager.loadOntologyFromOntologyDocument(owlres)
  val ontology = manager.loadOntology(org.semanticweb.owlapi.model.IRI.create(ontologySource))

  val store = new DataStore(DataStore.StoreType.ParallelSimpleNN)

  store.setNumberOfThreads(1)

  val files = Array(new File("data.ttl"))
  store.importFiles(files)

  store.importOntology(ontology)
  store.applyReasoning()

  private def mkPrefixes(default: String): Prefixes = {
    val prefixes = new Prefixes
    prefixes.addPrefixes(Prefixes.DEFAULT_IMMUTABLE_INSTANCE)
    prefixes.declarePrefix(":", default)
    prefixes
  }

  // Ask a query, getting the respective boolean result for ask queries
  // and true for non-empty select queries as well.
  def ask(query: String): Boolean = {
    val prefixes = mkPrefixes(ontologySource)
    val tupleIterator = store.compileQuery(query, prefixes)
    tupleIterator.open()
    tupleIterator.isValid
  }

  // Run query and get a list of IRI (for select queries).
  def run(query: String): List[IRI] = {
    val prefixes = mkPrefixes(ontologySource)
    val tupleIterator = store.compileQuery(query, prefixes)
    val arity = tupleIterator.getArity
    var multiplicity = tupleIterator.open()
    val lb = scala.collection.mutable.ListBuffer.empty[IRI]
    while(multiplicity != 0) {
      var termIndex = 0
      while (termIndex < arity) {
        val resource = tupleIterator.getResource(termIndex)
        lb += IRI(resource.toString(prefixes))
        termIndex = termIndex + 1
      }
      multiplicity = tupleIterator.advance()
    }
    lb.toList
  }
}
