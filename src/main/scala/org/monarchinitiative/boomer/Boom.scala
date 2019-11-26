package org.monarchinitiative.boomer

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.{Axiom, ConceptInclusion, Reasoner, ReasonerState}
import zio._

import scala.Ordering.Double.TotalOrdering
import scala.annotation.tailrec

object Boom {

  final case class Proposal(label: String, axioms: Set[ConceptInclusion], probability: Double)

  final case class AlternativesGroup(groups: Set[Proposal]) {

    val mostProbable: Proposal = groups.maxBy(_.probability)

  }

  final case class SelectedProposal(selected: Proposal, remainingAlternatives: List[Proposal], remainingPossibilities: List[AlternativesGroup], previousReasonerState: ReasonerState)

  def evaluate(assertions: Set[Axiom], probabilisticOntology: Set[AlternativesGroup]): Task[(List[SelectedProposal], ReasonerState)] = {
    val whelk = Reasoner.assert(assertions)
    println("Done first classification")
    val orderedHypotheticals = probabilisticOntology.toList.sortBy(_.mostProbable.probability)(Ordering[Double].reverse)
    val maxProbability = orderedHypotheticals.map(ah => Math.log(ah.mostProbable.probability)).sum
    orderedHypotheticals match {
      case first :: rest =>
        val alternativesByDescreasingProb = first.groups.toList.sortBy(_.probability)(Ordering[Double].reverse)
        alternativesByDescreasingProb match {
          case newSelection :: others =>
            search1(Some(SelectedProposal(newSelection, others, rest, whelk)), Nil).map { case (selectedHypotheticals, reasonerState) =>
              println(s"Max probability: $maxProbability")
              val jointProbability = selectedHypotheticals.map(sh => Math.log(sh.selected.probability)).sum
              println(jointProbability)
              (selectedHypotheticals, reasonerState)
            }
          case Nil                    => ZIO.fail(BoomError("Empty alternative set should not exist"))
        }
      case Nil           => ZIO.fail(BoomError("Nothing to do"))
    }
  }

  @tailrec
  def search1(current: Option[SelectedProposal], previouslySelected: List[SelectedProposal]): Task[(List[SelectedProposal], ReasonerState)] = {
    println(previouslySelected.size)
    //println(current)
    current match {
      case Some(vah @ SelectedProposal(selected, others, remaining, prevReasonerState)) =>
        val newReasonerState = Reasoner.assert(selected.axioms, prevReasonerState)
        if (isIncoherent(newReasonerState))
          search1(nextHypothetical(vah), previouslySelected)
        else {
          remaining match {
            case newGroup :: remainingGroups =>
              val alternativesByDescreasingProb = newGroup.groups.toList.sortBy(_.probability)(Ordering[Double].reverse)
              alternativesByDescreasingProb match {
                case newSelection :: rest =>
                  search1(Some(SelectedProposal(newSelection, rest, remainingGroups, newReasonerState)), vah :: previouslySelected)
                case Nil                  => ZIO.fail(BoomError("Empty alternative set should not exist"))
              }
            case Nil                         => ZIO.succeed(vah :: previouslySelected, newReasonerState)
          }
        }
      case None                                                                         =>
        previouslySelected match {
          case oldVAH :: older => search1(nextHypothetical(oldVAH), older)
          case Nil             => ZIO.fail(BoomError("Found no result"))
        }
    }
  }

  private def nextHypothetical(vah: SelectedProposal): Option[SelectedProposal] = {
    vah.remainingAlternatives match {
      case newSelection :: rest => Some(SelectedProposal(newSelection, rest, vah.remainingPossibilities, vah.previousReasonerState))
      case Nil                  => None
    }
  }

  private def isIncoherent(state: ReasonerState): Boolean = {
    //    val bad = state.closureSubsBySuperclass(Bottom).filter(t => !t.isAnonymous && t != Bottom).map(_.toString)
    //    if (bad.nonEmpty) println(bad.mkString("  "))
    state.closureSubsBySuperclass(Bottom).exists(t => !t.isAnonymous && t != Bottom)
  }

  final case class BoomError(message: String) extends Throwable(message)

}
