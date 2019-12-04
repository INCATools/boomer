package org.monarchinitiative.boomer

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk._
import zio._

import scala.Ordering.Double.TotalOrdering
import scala.annotation.tailrec

object Boom {

  sealed trait GeneralizedProposal {

    def axioms: Set[ConceptInclusion]

  }

  final case class Proposal(label: String, axioms: Set[ConceptInclusion], probability: Double) extends GeneralizedProposal

  sealed trait Possibility {

    def sorted: List[GeneralizedProposal]

  }

  final case class AlternativesGroup(groups: Set[Proposal]) extends Possibility {

    val mostProbable: Proposal = groups.maxBy(_.probability)

    val sorted: List[Proposal] = groups.toList.sortBy(_.probability)(Ordering[Double].reverse)

  }

  final case class ClumpProposal(proposal: Map[AlternativesGroup, Proposal]) extends GeneralizedProposal {

    def axioms: Set[ConceptInclusion] = proposal.values.flatMap(_.axioms).toSet

  }

  final case class Clump(groups: Set[AlternativesGroup]) extends Possibility {

    //TODO this may grow too large
    def sorted: List[ClumpProposal] =
      groups.foldLeft(List(Map.empty[AlternativesGroup, Proposal])) { case (acc, group) =>
        group.groups.toList.flatMap(p => acc.map(m => m.updated(group, p)))
      }.sortBy(_.values.map(_.probability).product)(Ordering[Double].reverse).map(ClumpProposal)

    def add(possibility: Possibility): Clump = possibility match {
      case ag: AlternativesGroup => Clump(groups + ag)
      case Clump(gs)             => Clump(groups ++ gs)
    }

  }

  sealed trait Selection {

    def remainingPossibilities: List[Possibility]

    def reasonerState: ReasonerState

    def probability: Double

  }

  final case class Init(remainingPossibilities: List[AlternativesGroup], reasonerState: ReasonerState) extends Selection {

    override val probability: Double = 1.0

  }

  final case class SelectedProposal(selected: Proposal, group: AlternativesGroup, remainingPossibilities: List[Possibility], reasonerState: ReasonerState) extends Selection {

    override def probability: Double = selected.probability

  }

  final case class SelectedClump(selected: ClumpProposal, clump: Clump, remainingPossibilities: List[Possibility], reasonerState: ReasonerState) extends Selection {

    override def probability: Double = selected.proposal.values.map(_.probability).product

  }

  def evaluate(assertions: Set[Axiom], probabilisticOntology: Set[AlternativesGroup]): Task[List[Selection]] = {
    val whelk = Reasoner.assert(assertions)
    println("Done first classification")
    val orderedHypotheticals = probabilisticOntology.toList.sortBy(_.mostProbable.probability)(Ordering[Double].reverse)
    val maxProbability = orderedHypotheticals.map(ah => Math.log(ah.mostProbable.probability)).sum
    search(List(Init(orderedHypotheticals, whelk))).map { selected =>
      println(s"Max probability: $maxProbability")
      val jointProbability = selected.map(s => Math.log(s.probability)).sum
      println(jointProbability)
      selected
    }
  }

  @tailrec
  def search(selected: List[Selection]): Task[List[Selection]] =
    selected match {
      case Nil       => ZIO.fail(BoomError("Search must be called with at least an Init in previouslySelected"))
      case prev :: _ =>
        prev.remainingPossibilities match {
          case Nil               => ZIO.succeed(selected)
          case next :: remaining => tryAdding(next, remaining, prev.reasonerState) match {
            case None            => searchForConflict(next, selected, remaining)
            case Some(selection) => search(selection :: selected)
          }
        }
    }

  @tailrec
  private def searchForConflict(possibility: Possibility, selected: List[Selection], newRemaining: List[Possibility]): Task[List[Selection]] =
    selected match {
      case Nil                             => ZIO.fail(BoomError("Reached beginning trying to find conflicting group"))
      case first :: (rest @ (second :: _)) =>
        val compatible = possibility.sorted.exists { proposal =>
          val newReasonerState = Reasoner.assert(proposal.axioms, second.reasonerState)
          isCoherent(newReasonerState)
        }
        if (compatible) {
          println(s"Making clump at ${rest.size}")
          val clump = first match {
            case SelectedProposal(_, ag, _, _)         => Clump(Set(ag)).add(possibility)
            case SelectedClump(_, c @ Clump(gs), _, _) => c.add(possibility)
          }
          println(clump)
          val updatedRemaining = newRemaining.filterNot(_ == possibility)
          tryAdding(clump, updatedRemaining, second.reasonerState) match {
            case None            => ZIO.fail(BoomError("We aren't prepared for clumps that can't be added!"))
            case Some(selection) => search(selection :: rest)
          }
        } else {
          val remainingPossibility = first match {
            case SelectedProposal(_, ag, _, _) => ag
            case SelectedClump(_, c, _, _)     => c
          }
          searchForConflict(possibility, rest, remainingPossibility :: newRemaining)
        }
    }

  private def tryAdding(possibility: Possibility, remaining: List[Possibility], reasonerState: ReasonerState): Option[Selection] = {
    possibility match {
      case ag: AlternativesGroup =>
        val maybeAdded = ag.sorted.to(LazyList).map { proposal =>
          val newReasonerState = Reasoner.assert(proposal.axioms, reasonerState)
          proposal -> newReasonerState
        }.find { case (_, state) => isCoherent(state) }
        maybeAdded.map {
          case (proposal, state) => SelectedProposal(proposal, ag, remaining, state)
        }

      case clump: Clump =>
        val maybeAdded = clump.sorted.to(LazyList).map { proposal =>
          val newReasonerState = Reasoner.assert(proposal.axioms, reasonerState)
          proposal -> newReasonerState
        }.find { case (_, state) => isCoherent(state) }
        maybeAdded.map {
          case (proposal, state) => SelectedClump(proposal, clump, remaining, state)
        }
    }
  }

  private def jointProbability(selections: List[SelectedProposal]): Double = selections.map(s => Math.log(s.selected.probability)).sum

  private def isCoherent(state: ReasonerState): Boolean = !isIncoherent(state)

  private def isIncoherent(state: ReasonerState): Boolean = state.closureSubsBySuperclass(Bottom).exists(t => !t.isAnonymous && t != Bottom)

  private def unsatisfiableClasses(state: ReasonerState): Set[AtomicConcept] =
    state.closureSubsBySuperclass(Bottom).collect { case term: AtomicConcept if term != Bottom => term }

  final case class BoomError(message: String) extends Throwable(message)

}
