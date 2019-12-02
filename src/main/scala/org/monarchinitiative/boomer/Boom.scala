package org.monarchinitiative.boomer

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.{AtomicConcept, Axiom, ConceptInclusion, Entity, Reasoner, ReasonerState}
import zio._

import scala.Ordering.Double.TotalOrdering
import scala.annotation.tailrec

object Boom {

  final case class Proposal(label: String, axioms: Set[ConceptInclusion], probability: Double)

  final case class AlternativesGroup(groups: Set[Proposal]) {

    val mostProbable: Proposal = groups.maxBy(_.probability)

    val sorted: List[Proposal] = groups.toList.sortBy(_.probability)(Ordering[Double].reverse)

  }

  final case class SelectedProposal(selected: Proposal, group: AlternativesGroup, remainingAlternatives: List[Proposal], remainingPossibilities: List[AlternativesGroup], previousReasonerState: ReasonerState)

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
            search(Some(SelectedProposal(newSelection, first, others, rest, whelk)), Nil, whelk, None).map { case (selectedHypotheticals, reasonerState) =>
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

  //@tailrec
  def search(current: Option[SelectedProposal], previouslySelected: List[SelectedProposal], lastReasonerState: ReasonerState, lastTried: Option[SelectedProposal]): Task[(List[SelectedProposal], ReasonerState)] = {
    //println(previouslySelected.size)
    (current, lastTried) match {
      case (Some(vah @ SelectedProposal(selected, _, _, remaining, prevReasonerState)), _) =>
        val newReasonerState = Reasoner.assert(selected.axioms, prevReasonerState)
        if (isIncoherent(newReasonerState))
          search(nextHypothetical(vah), previouslySelected, newReasonerState, Some(vah))
        else {
          remaining match {
            case newGroup :: remainingGroups =>
              val alternativesByDescreasingProb = newGroup.groups.toList.sortBy(_.probability)(Ordering[Double].reverse)
              alternativesByDescreasingProb match {
                case newSelection :: rest =>
                  search(Some(SelectedProposal(newSelection, newGroup, rest, remainingGroups, newReasonerState)), vah :: previouslySelected, newReasonerState, Some(vah))
                case Nil                  => ZIO.fail(BoomError("Empty alternative set should not exist"))
              }
            case Nil                         => ZIO.succeed(vah :: previouslySelected, newReasonerState)
          }
        }
      case (None, maybeLast)                                                               =>
        //        val unsats = unsatisfiableClasses(lastReasonerState).toSet[Entity]
        //        val res = previouslySelected.find(_.selected.axioms.exists(ax => ax.signature.exists(unsats)))
        //        println(unsats)
        //        println(s"DO SOMETHING WITH: $res")


        println(s"HIT WALL at ${maybeLast.map(_.selected.label)}")
        val newBeginnings = for {
          SelectedProposal(nextSelected, nextGroup, nextOthers, _, _) <- maybeLast.toList
          //(selectionToUse, othersToUse) <- if (shuffled) nextOthers.headOption.map(_ -> nextOthers.tail) else Some(nextSelected -> nextOthers)
          SelectedProposal(_, group, _, remainingPossibilities, previousReasonerState) <- previouslySelected.lastOption.toList
          filteredRemainingPossibilities = remainingPossibilities.filterNot(_ == nextGroup)
          proposal <- nextGroup.sorted
        } yield {
          val selection = SelectedProposal(proposal, nextGroup, Nil, group :: filteredRemainingPossibilities, previousReasonerState)
          ZIO.effect(search(Some(selection), Nil, previousReasonerState, None))
        }
        if (newBeginnings.isEmpty) ZIO.fail(BoomError("Not sure what to do"))
        else {
          val res = ZIO.collectAllPar(newBeginnings)
          for {
            collected <- ZIO.collectAllPar(newBeginnings)
            results <- ZIO.sequence(collected)
            best = results.maxBy(res => jointProbability(res._1))
          } yield best
        }



      //old backtracking
      //search1(nextHypothetical(oldVAH), older, lastReasonerState)
      //case Nil => ZIO.fail(BoomError("Found no result"))

    }
  }

  private def jointProbability(selections: List[SelectedProposal]): Double = selections.map(s => Math.log(s.selected.probability)).sum

  private def nextHypothetical(vah: SelectedProposal): Option[SelectedProposal] = {
    vah.remainingAlternatives match {
      case newSelection :: rest => Some(SelectedProposal(newSelection, vah.group, rest, vah.remainingPossibilities, vah.previousReasonerState))
      case Nil                  => None
    }
  }

  private def isIncoherent(state: ReasonerState): Boolean = {
    //    val bad = state.closureSubsBySuperclass(Bottom).filter(t => !t.isAnonymous && t != Bottom).map(_.toString)
    //    if (bad.nonEmpty) println(bad.mkString("  "))
    state.closureSubsBySuperclass(Bottom).exists(t => !t.isAnonymous && t != Bottom)
  }

  private def unsatisfiableClasses(state: ReasonerState): Set[AtomicConcept] =
    state.closureSubsBySuperclass(Bottom).collect { case term: AtomicConcept if term != Bottom => term }

  final case class BoomError(message: String) extends Throwable(message)

}
