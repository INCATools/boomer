package org.monarchinitiative.boomer

import java.io.File

import org.geneontology.whelk._
import org.monarchinitiative.boomer.Model.{ProbabilisticOntology, Proposal, Uncertainty}
import zio.test.Assertion._
import zio.test._

object TestOntUtilData {

  val TestOntPrefix = "http://example.org/ontutil"

  val assertedOnt: Set[Axiom] = Set(
    ConceptInclusion(AtomicConcept(s"$TestOntPrefix/B"), AtomicConcept(s"$TestOntPrefix/A")),
    ConceptInclusion(AtomicConcept(s"$TestOntPrefix/C"), AtomicConcept(s"$TestOntPrefix/B")),
    ConceptInclusion(AtomicConcept(s"$TestOntPrefix/X"), AtomicConcept(s"$TestOntPrefix/Y")),
    ConceptInclusion(Nominal(Individual("http://example.org/ontutil/uncertainty1")), AtomicConcept("http://boom.monarchinitiative.org/vocab/Uncertainty")),
    ConceptInclusion(Nominal(Individual("http://example.org/ontutil/uncertainty2")), AtomicConcept("http://boom.monarchinitiative.org/vocab/Uncertainty")),
    ConceptInclusion(Nominal(Individual("http://example.org/ontutil/proposal1-1")), AtomicConcept("http://boom.monarchinitiative.org/vocab/Proposal")),
    ConceptInclusion(Nominal(Individual("http://example.org/ontutil/proposal1-2")), AtomicConcept("http://boom.monarchinitiative.org/vocab/Proposal")),
    ConceptInclusion(Nominal(Individual("http://example.org/ontutil/proposal1-3")), AtomicConcept("http://boom.monarchinitiative.org/vocab/Proposal")),
    ConceptInclusion(Nominal(Individual("http://example.org/ontutil/proposal2-1")), AtomicConcept("http://boom.monarchinitiative.org/vocab/Proposal")),
    ConceptInclusion(Nominal(Individual("http://example.org/ontutil/proposal2-2")), AtomicConcept("http://boom.monarchinitiative.org/vocab/Proposal")),
    ConceptInclusion(Nominal(Individual("http://example.org/ontutil/proposal2-3")), AtomicConcept("http://boom.monarchinitiative.org/vocab/Proposal")))

  val uncertainties: Set[Uncertainty] = Set(
    Uncertainty(Set(
      Proposal("Y SubClassOf B", Set(ConceptInclusion(AtomicConcept(s"$TestOntPrefix/Y"), AtomicConcept(s"$TestOntPrefix/B"))), 0.4),
      Proposal("Y SuperClassOf B", Set(ConceptInclusion(AtomicConcept(s"$TestOntPrefix/B"), AtomicConcept(s"$TestOntPrefix/Y"))), 0.2),
      Proposal("Y Equiv B", Set(ConceptInclusion(AtomicConcept(s"$TestOntPrefix/Y"), AtomicConcept(s"$TestOntPrefix/B")), ConceptInclusion(AtomicConcept(s"$TestOntPrefix/B"), AtomicConcept(s"$TestOntPrefix/Y"))), 0.4)
    )),
    Uncertainty(Set(
      Proposal("X SubClassOf A", Set(
        ConceptInclusion(AtomicConcept(s"$TestOntPrefix/X"), AtomicConcept(s"$TestOntPrefix/A")),
        ConceptInclusion(AtomicConcept(s"$TestOntPrefix/XASib"), AtomicConcept(s"$TestOntPrefix/A")),
        // direction of this conjunction is not deterministic; could cause test failure
        ConceptInclusion(Conjunction(AtomicConcept(s"$TestOntPrefix/X"), AtomicConcept(s"$TestOntPrefix/XASib")), BuiltIn.Bottom)
      ), 0.1),
      Proposal("X SuperClassOf A", Set(ConceptInclusion(AtomicConcept(s"$TestOntPrefix/A"), AtomicConcept(s"$TestOntPrefix/X"))), 0.8),
      Proposal("X Equiv A", Set(ConceptInclusion(AtomicConcept(s"$TestOntPrefix/X"), AtomicConcept(s"$TestOntPrefix/A")), ConceptInclusion(AtomicConcept(s"$TestOntPrefix/A"), AtomicConcept(s"$TestOntPrefix/X"))), 0.1)
    ))
  )

}

object TestOntUtil extends DefaultRunnableSpec {

  def spec = suite("TestOntUtil")(
    testM("Probabilistic ontology can be loaded from OWL API") {
      for {
        probOnt <- OntUtil.readProbabilisticOntology(new File("src/test/resources/prob_ont_1.ofn"))
      } yield assert(probOnt)(equalTo(ProbabilisticOntology(TestOntUtilData.assertedOnt, TestOntUtilData.uncertainties)))
    }
  )
}