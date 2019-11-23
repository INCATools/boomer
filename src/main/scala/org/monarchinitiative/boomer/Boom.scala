package org.monarchinitiative.boomer

import java.util.UUID

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.{AtomicConcept, Axiom, ConceptInclusion, Conjunction, Reasoner, ReasonerState}
import zio._

import scala.Ordering.Double.TotalOrdering
import scala.annotation.tailrec

object Boom {

  final case class HypotheticalAxioms(label: String, axioms: Set[ConceptInclusion], probability: Double)

  final case class AlternativeHypotheticals(groups: Set[HypotheticalAxioms]) {

    val mostProbable: HypotheticalAxioms = groups.maxBy(_.probability)

  }

  def evaluate(assertions: Set[Axiom], probabilisticOntology: Set[AlternativeHypotheticals]): Task[(List[HypotheticalAxioms], ReasonerState)] = {
    val whelk = Reasoner.assert(assertions)
    println("Done first classification")
    val orderedHypotheticals = probabilisticOntology.toList.sortBy(_.mostProbable.probability)(Ordering[Double].reverse)
    search(orderedHypotheticals, Nil, whelk).map { case (selectedHypotheticals, reasonerState) =>
      val jointProbability = selectedHypotheticals.map(_.probability).product
      (selectedHypotheticals, reasonerState)
    }
  }

  /**
   * @return (accepted hypothetical axioms, final reasoner state)
   */
  @tailrec
  def search(possibilities: List[AlternativeHypotheticals], previouslySelected: List[HypotheticalAxioms], reasonerState: ReasonerState): Task[(List[HypotheticalAxioms], ReasonerState)] = {
    possibilities match {
      case Nil                                   => ZIO.succeed(previouslySelected, reasonerState)
      case current :: remainingAlternativeGroups =>
        val alternativesByDescreasingProb = current.groups.toList.sortBy(_.probability)(Ordering[Double].reverse)
        val maybeSelectedAlternative =
          alternativesByDescreasingProb.to(LazyList)
            .map {
              group =>
                group -> Reasoner.assert(group.axioms, reasonerState)
            }
            .find {
              case (_, state) => !isIncoherent(state)
            }
        maybeSelectedAlternative match {
          case None                         => ZIO.fail(BoomError(s"Nothing worked in group: $current"))
          case Some((selectedGroup, state)) => search(remainingAlternativeGroups, selectedGroup :: previouslySelected, state)
        }
    }
  }

  private def isIncoherent(state: ReasonerState): Boolean = state.closureSubsBySuperclass(Bottom).exists(t => !t.isAnonymous && t != Bottom)

  final case class BoomError(message: String) extends Throwable(message)

}
