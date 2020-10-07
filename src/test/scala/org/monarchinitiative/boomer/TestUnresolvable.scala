package org.monarchinitiative.boomer

import org.geneontology.whelk.{AtomicConcept, BuiltIn, ConceptInclusion, Conjunction}
import org.monarchinitiative.boomer.Boom.UnresolvableUncertainties
import org.monarchinitiative.boomer.Model.{Proposal, Uncertainty}
import org.monarchinitiative.boomer.TestUtil._
import zio.test.Assertion._
import zio.test._

object TestUnresolvable extends DefaultRunnableSpec {

  def spec = suite("TestUnresolvable")(
    testM("Ensure reasonable behavior when uncertainties are unresolvable") {
      val resultIO = for {
        axioms <- loadTestAxiomsFromFile("unresolvable-mappings-test1.ofn")
        result <- Boom.evaluate(axioms, uncertainties1, Set.empty, 10, 1)
      } yield result
      assertM(resultIO.flip)(equalTo(UnresolvableUncertainties))
    },
    testM("Ensure a perplexity can be created if there is a conflict with the first uncertainty") {
      for {
        axioms <- loadTestAxiomsFromFile("unresolvable-mappings-test2.ofn")
        result <- Boom.evaluate(axioms, uncertainties2, Set.empty, 10, 1)
      } yield assert(result)(isNonEmpty)
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
