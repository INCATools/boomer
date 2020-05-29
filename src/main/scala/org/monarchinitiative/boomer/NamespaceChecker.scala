package org.monarchinitiative.boomer

import org.geneontology.whelk.Reasoner.QueueDelegate
import org.geneontology.whelk._

final case class NamespaceChecker(prohibitedEquivalenceNamespaces: Set[String], violations: List[(AtomicConcept, AtomicConcept)])
    extends QueueDelegate {

  def processConcept(concept: Concept, reasoner: ReasonerState): ReasonerState = reasoner

  def processConceptInclusion(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = namespaceCheck(ci, reasoner)

  def processSubPlus(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = namespaceCheck(ci, reasoner)

  def processLink(link: Link, reasoner: ReasonerState): ReasonerState = reasoner

  private[this] def namespaceCheck(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(sub @ AtomicConcept(subID), sup @ AtomicConcept(supID)) if sub != sup =>
      var currentViolations = violations
      if (reasoner.closureSubsBySuperclass.getOrElse(sub, Set.empty)(sup))
        for {
          namespace <- prohibitedEquivalenceNamespaces
          if subID.startsWith(namespace) && supID.startsWith(namespace)
        } currentViolations = (sub, sup) :: currentViolations
      val newChecker = this.copy(violations = currentViolations)
      val newDelegates = reasoner.queueDelegates.updated(NamespaceChecker.DelegateKey, newChecker)
      reasoner.copy(queueDelegates = newDelegates)
    case _ => reasoner
  }

}

object NamespaceChecker {

  final val DelegateKey: String = "org.monarchinitiative.boomer.NamespaceChecker"

}
