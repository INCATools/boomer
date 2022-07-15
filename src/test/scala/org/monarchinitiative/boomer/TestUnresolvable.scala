package org.monarchinitiative.boomer

import org.geneontology.whelk._
import org.monarchinitiative.boomer.Boom.UnresolvableUncertainties
import org.monarchinitiative.boomer.Model.{Proposal, Uncertainty}
import org.monarchinitiative.boomer.TestUtil._
import zio.test.Assertion._
import zio.test._

object TestUnresolvable extends ZIOSpecDefault {

  def spec = suite("TestUnresolvable")(
    test("Ensure reasonable behavior when uncertainties are unresolvable") {
      val resultIO = for {
        axioms <- loadTestAxiomsFromFile("unresolvable-mappings-test1.ofn")
        delegates = Map(NamespaceChecker.DelegateKey -> NamespaceChecker(Set.empty, Nil)) //TODO handle error if this is not provided
        whelk = Reasoner.assert(axioms, delegates)
        result <- Boom.evaluateGreedily(uncertainties1, whelk, 10, 1)
      } yield result
      assertZIO(resultIO.flip)(equalTo(UnresolvableUncertainties))
    },
    test("Ensure a perplexity can be created if there is a conflict with the first uncertainty") {
      for {
        axioms <- loadTestAxiomsFromFile("unresolvable-mappings-test2.ofn")
        delegates = Map(NamespaceChecker.DelegateKey -> NamespaceChecker(Set.empty, Nil))
        whelk = Reasoner.assert(axioms, delegates)
        result <- Boom.evaluateGreedily(uncertainties2, whelk, 10, 1)
      } yield assertTrue(result.nonEmpty)
    }
  )

  val TestOntPrefix = "http://example.org/test"

  // These uncertainties can't be resolved against the ontology
  val uncertainties1: Set[Uncertainty] = Set(
    Uncertainty(
      Set(
        Proposal(
          "A2 Equiv B2",
          Set(
            ConceptInclusion(AtomicConcept(s"$TestOntPrefix/A2"), AtomicConcept(s"$TestOntPrefix/B2")),
            ConceptInclusion(AtomicConcept(s"$TestOntPrefix/B2"), AtomicConcept(s"$TestOntPrefix/A2"))
          ),
          0.5
        )
      )
    ),
    Uncertainty(
      Set(
        Proposal(
          "A3 Equiv B3",
          Set(
            ConceptInclusion(AtomicConcept(s"$TestOntPrefix/A3"), AtomicConcept(s"$TestOntPrefix/B3")),
            ConceptInclusion(AtomicConcept(s"$TestOntPrefix/B3"), AtomicConcept(s"$TestOntPrefix/A3"))
          ),
          0.99
        )
      )
    )
  )

  // A perplexity must be made with the first uncertainty
  val uncertainties2: Set[Uncertainty] = Set(
    Uncertainty(
      Set(
        Proposal(
          "A2 Equiv B2",
          Set(
            ConceptInclusion(AtomicConcept(s"$TestOntPrefix/A2"), AtomicConcept(s"$TestOntPrefix/B2")),
            ConceptInclusion(AtomicConcept(s"$TestOntPrefix/B2"), AtomicConcept(s"$TestOntPrefix/A2"))
          ),
          0.5
        )
      )
    ),
    Uncertainty(
      Set(
        Proposal("A3 Equiv B3",
                 Set(ConceptInclusion(Conjunction(AtomicConcept(s"$TestOntPrefix/A1"), AtomicConcept(s"$TestOntPrefix/B1")), BuiltIn.Bottom)),
                 0.99),
        Proposal(
          "A3 Equiv B3",
          Set(
            ConceptInclusion(AtomicConcept(s"$TestOntPrefix/A3"), AtomicConcept(s"$TestOntPrefix/B3")),
            ConceptInclusion(AtomicConcept(s"$TestOntPrefix/B3"), AtomicConcept(s"$TestOntPrefix/A3"))
          ),
          0.01
        )
      )
    )
  )

}
