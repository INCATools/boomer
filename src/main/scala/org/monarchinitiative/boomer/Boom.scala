package org.monarchinitiative.boomer

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.{Axiom, ConceptInclusion, Reasoner, ReasonerState}
import zio._

import scala.Ordering.Double.TotalOrdering
import scala.annotation.tailrec

object Boom {

  final case class HypotheticalAxioms(label: String, axioms: Set[ConceptInclusion], probability: Double)

  final case class AlternativeHypotheticals(groups: Set[HypotheticalAxioms]) {

    val mostProbable: HypotheticalAxioms = groups.maxBy(_.probability)

  }

  final case class VisitedAlternativeHypotheticals(selected: HypotheticalAxioms, remainingAlternatives: List[HypotheticalAxioms], remainingPossibilities: List[AlternativeHypotheticals], previousReasonerState: ReasonerState)

  def evaluate(assertions: Set[Axiom], probabilisticOntology: Set[AlternativeHypotheticals]): Task[(List[VisitedAlternativeHypotheticals], ReasonerState)] = {
    val whelk = Reasoner.assert(assertions)
    println("Done first classification")
    val orderedHypotheticals = probabilisticOntology.toList.sortBy(_.mostProbable.probability)(Ordering[Double].reverse)
    val maxProbability = orderedHypotheticals.map(ah => Math.log(ah.mostProbable.probability)).sum
    search(Nil, orderedHypotheticals, Nil, whelk).map { case (selectedHypotheticals, reasonerState) =>
      println(s"Max probability: $maxProbability")
      val jointProbability = selectedHypotheticals.map(sh => Math.log(sh.selected.probability)).sum
      println(jointProbability)
      (selectedHypotheticals, reasonerState)
    }
  }

  /**
   * @return (accepted hypothetical axioms, final reasoner state)
   */
  @tailrec
  def search(currentAlternatives: List[HypotheticalAxioms], possibilities: List[AlternativeHypotheticals], previouslySelected: List[VisitedAlternativeHypotheticals], reasonerState: ReasonerState): Task[(List[VisitedAlternativeHypotheticals], ReasonerState)] = {
    println(previouslySelected.size)
    val coherent = !isIncoherent(reasonerState)
    (coherent, currentAlternatives, possibilities, previouslySelected) match {
      case (true, Nil, Nil, ps)                                                                                                                                                                     =>
        ZIO.succeed((ps, reasonerState))
      case (true, Nil, current :: remainingAlternativeGroups, ps)                                                                                                                                   =>
        val alternativesByDescreasingProb = current.groups.toList.sortBy(_.probability)(Ordering[Double].reverse)
        search(alternativesByDescreasingProb, remainingAlternativeGroups, ps, reasonerState)
      case (true, selected :: remainingAlternatives, remainingPossibilities, ps)                                                                                                                    =>
        val newReasonerState = Reasoner.assert(selected.axioms, reasonerState)
        search(Nil, remainingPossibilities, VisitedAlternativeHypotheticals(selected, remainingAlternatives, remainingPossibilities, reasonerState) :: ps, newReasonerState)
      case (false, _, _, VisitedAlternativeHypotheticals(_, Nil, _, _) :: VisitedAlternativeHypotheticals(_, othersToSelect, remainingPossibilities, prevReasonerState) :: otherPreviouslySelected) =>
        search(othersToSelect, remainingPossibilities, otherPreviouslySelected, reasonerState) ///
      case (false, _, _, VisitedAlternativeHypotheticals(_, othersToSelect, remainingPossibilities, prevReasonerState) :: otherPreviouslySelected)                                                  =>
        search(othersToSelect, remainingPossibilities, otherPreviouslySelected, prevReasonerState)
      case (false, _, _, Nil)                                                                                                                                                                       =>
        ZIO.fail(BoomError("Found no result"))
    }
  }

  private def isIncoherent(state: ReasonerState): Boolean = state.closureSubsBySuperclass(Bottom).exists(t => !t.isAnonymous && t != Bottom)

  final case class BoomError(message: String) extends Throwable(message)

}
