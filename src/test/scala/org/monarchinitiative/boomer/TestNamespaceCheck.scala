package org.monarchinitiative.boomer

import org.geneontology.whelk._
import org.monarchinitiative.boomer.TestUtil._
import zio.test._

object TestNamespaceCheck extends ZIOSpecDefault {

  def spec = suite("TestNamespaceCheck")(
    test("Prevent terms in same namespace from being inferred equivalent") {
      for {
        axioms <- loadTestAxiomsFromFile("namespace-check.ofn")
        whelk = Reasoner.assert(axioms, Map(NamespaceChecker.DelegateKey -> NamespaceChecker(Set("http://example.org/namespacecheck1/"), Nil)))
        violations = namespaceViolations(whelk)
      } yield assertTrue(violations(AtomicConcept("http://example.org/namespacecheck1/A"))) &&
        assertTrue(violations(AtomicConcept("http://example.org/namespacecheck1/D"))) &&
        assertTrue(!(violations(AtomicConcept("http://example.org/namespacecheck2/G")))) &&
        assertTrue(!(violations(AtomicConcept("http://example.org/namespacecheck2/H"))))
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
