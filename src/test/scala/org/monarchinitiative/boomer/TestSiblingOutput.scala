package org.monarchinitiative.boomer

import org.geneontology.whelk.{AtomicConcept, ReasonerState}
import org.monarchinitiative.boomer.Main.{reasonerWithNamespaceChecker, resolvedUncertaintiesAsOntology}
import org.monarchinitiative.boomer.OntUtil.{asOBOGraphs, VizWidth}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom
import zio._
import zio.test._

import scala.jdk.CollectionConverters._

object TestSiblingOutput extends DefaultRunnableSpec {

  private val probs = "A:1\tB:1\t0.01\t0.01\t0.05\t0.93\nA:2\tB:1\t0.01\t0.01\t0.95\t0.03"
  private val prefixes = Map("A" -> "http://example.org/A/", "B" -> "http://example.org/B/")
  private val A1 = AtomicConcept("http://example.org/A/1")
  private val B1 = AtomicConcept("http://example.org/B/1")

  def spec = suite("TestSiblingOutput") {
    testM("Selected sibling proposals should result in an annotation property relation and an edge in the OBOgraph") {
      for {
        mappings <- ZIO.foreach(probs.split("\n", -1).toSet)(Mapping.parsePTableLine(_, prefixes))
        prohibitedPrefixEquivalences = prefixes.values.to(Set)
        whelk = ReasonerState.empty
        whelkN = reasonerWithNamespaceChecker(whelk, prohibitedPrefixEquivalences)
        results <- Boom.evaluateExhaustively(mappings.map(_.uncertainty), whelkN, 1)
        best = results.head
        proposals = best.uncertainties.values.map(_._1).to(Set)
        a1b1 = Siblings(A1, B1)
        ont = resolvedUncertaintiesAsOntology(best, whelkN, Map.empty, "test")
        siblingAxioms = ont.getAxioms().asScala.to(Set).collect { case ax @ AnnotationAssertion(_, OntUtil.SiblingOf, _, _) =>
          ax
        }
        obograph = asOBOGraphs(ont, Map.empty, prefixes)
        fullOwlAxioms = proposals.flatMap(_.axioms).flatMap(OntUtil.whelkToOWL(_, false).toSet)
      } yield assertTrue(proposals.size == 2) &&
        assertTrue(proposals.exists(_.label == a1b1)) &&
        assertTrue(siblingAxioms.size == 1) &&
        assertTrue(siblingAxioms.head.getAnnotations(VizWidth).asScala.to(Set).nonEmpty) &&
        assertTrue(obograph.graphs.head.nodes.to(Set).size == 3) &&
        assertTrue(obograph.graphs.head.edges.to(Set).size == 2) &&
        assertTrue(fullOwlAxioms.exists(_.isInstanceOf[OWLDisjointClassesAxiom]))
    }
  }

}
