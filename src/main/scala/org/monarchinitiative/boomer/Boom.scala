package org.monarchinitiative.boomer

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk._
import org.monarchinitiative.boomer.Model.{Selection, _}
import org.monarchinitiative.boomer.Util.BisectSearchOp
import zio._
import zio.random.Random

import scala.Ordering.Double.TotalOrdering
import scala.annotation.tailrec
import scala.collection.Searching.InsertionPoint
import scala.math.Ordering

object Boom {

  def evaluate(assertions: Set[Axiom], probabilisticOntology: Set[AlternativesGroup]): Task[List[Selection]] = {
    val whelk = Reasoner.assert(assertions)
    println("Done first classification")
    //    Random.Live.random.shuffle(probabilisticOntology.toList).flatMap { shuffledHypotheticals =>
    //      val orderedHypotheticals = shuffledHypotheticals.sortBy(_.mostProbable.probability)(Ordering[Double].reverse)
    //      //val orderedHypotheticals = shuffledHypotheticals
    //      val maxProbability = orderedHypotheticals.map(ah => Math.log(ah.mostProbable.probability)).sum
    //      search(List(Init(orderedHypotheticals, whelk))).map { selected =>
    //        println(s"Max probability: $maxProbability")
    //        val jointProbability = selected.map(s => Math.log(s.probability)).sum
    //        println(jointProbability)
    //        selected
    //      }
    //    }
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
            case None            => searchForConflict(next, selected, remaining) match {
              case None              => ZIO.fail(BoomError("We aren't prepared for clumps that can't be added!"))
              case Some(nowSelected) => search(nowSelected)
            }
            case Some(selection) => search(selection :: selected)
          }
        }
    }

  private def searchForConflict(possibility: Possibility, selected: List[Selection], newRemaining: List[Possibility]): Option[List[Selection]] = {
    val InsertionPoint(index) = selected.bisect { selection =>
      possibility.sorted.exists { proposal =>
        val newReasonerState = Reasoner.assert(proposal.axioms, selection.reasonerState)
        isIncoherent(newReasonerState)
      }
    }
    //FIXME check for index edge cases
    val (newlyRemaining, stillSelectedPlusConflict) = selected.splitAt(index - 1)
    val conflict :: stillSelected = stillSelectedPlusConflict
    println(s"Making clump at $index")
    val clump = conflict match {
      case SelectedProposal(_, ag, _, _)         => Clump(Set(ag)).add(possibility)
      case SelectedClump(_, c @ Clump(gs), _, _) => c.add(possibility)
    }
    println(clump)
    val newlyRemainingPossibilties = newlyRemaining.map {
      case SelectedProposal(_, ag, _, _) => ag
      case SelectedClump(_, c, _, _)     => c
    }
    val updatedRemaining = newlyRemainingPossibilties.reverse ::: newRemaining
    tryAdding(clump, updatedRemaining, stillSelected.head.reasonerState).map(_ :: stillSelected)
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
