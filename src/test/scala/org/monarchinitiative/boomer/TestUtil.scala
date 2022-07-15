package org.monarchinitiative.boomer

import org.geneontology.whelk.{Axiom, Bridge}
import org.semanticweb.owlapi.apibinding.OWLManager
import zio._

object TestUtil {

  def loadTestAxiomsFromFile(fileName: String): Task[Set[Axiom]] = for {
    manager <- ZIO.attempt(OWLManager.createOWLOntologyManager())
    ontology <- ZIO.attempt(manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(fileName)))
  } yield Bridge.ontologyToAxioms(ontology)

}
