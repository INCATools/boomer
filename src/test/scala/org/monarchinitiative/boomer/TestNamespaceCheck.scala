package org.monarchinitiative.boomer

import org.geneontology.whelk._
import org.monarchinitiative.boomer.TestUtil._
import zio.test.Assertion._
import zio.test._

object TestNamespaceCheck extends DefaultRunnableSpec {

  def spec = suite("TestNamespaceCheck")(
    testM("Prevent terms in same namespace from being inferred equivalent") {
      for {
        axioms <- loadTestAxiomsFromFile("namespace-check.ofn")
        whelk = Reasoner.assert(axioms, Map(NamespaceChecker.DelegateKey -> NamespaceChecker(Set("http://example.org/namespacecheck1/"), Nil)))
        violations = namespaceViolations(whelk)
      } yield assert(violations(AtomicConcept("http://example.org/namespacecheck1/A")))(isTrue) &&
        assert(violations(AtomicConcept("http://example.org/namespacecheck1/D")))(isTrue) &&
        assert(violations(AtomicConcept("http://example.org/namespacecheck2/G")))(isFalse) &&
        assert(violations(AtomicConcept("http://example.org/namespacecheck2/H")))(isFalse)
    }
  )

  private def namespaceViolations(state: ReasonerState): Set[AtomicConcept] =
    state
      .queueDelegates(NamespaceChecker.DelegateKey)
      .asInstanceOf[NamespaceChecker]
      .violations
      .flatMap { case (left, right) =>
        Set(left, right)
      }
      .toSet

}
