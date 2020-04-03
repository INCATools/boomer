package org.monarchinitiative.boomer

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk._
import org.monarchinitiative.boomer.Model._
import org.monarchinitiative.boomer.Util.BisectSearchOp
import zio._
import zio.random.Random

import scala.Ordering.Double.TotalOrdering
import scala.annotation.tailrec
import scala.collection.Searching.InsertionPoint
import scala.math.Ordering

object Boom {

  def evaluate(assertions: Set[Axiom], uncertainties: Set[Uncertainty], shuffle: Boolean, prohibitedPrefixEquivalences: Set[String]): ZIO[Random, Throwable, List[Selection]] =
    for {
      uncertaintiesList <- if (shuffle) random.shuffle(uncertainties.toList) else ZIO.succeed(uncertainties.toList)
      orderedUncertainties = uncertaintiesList.sortBy(_.mostProbable.probability)(Ordering[Double].reverse)
      selections <- evaluateInOrder(assertions, orderedUncertainties, prohibitedPrefixEquivalences)
    } yield selections

  def evaluateInOrder(assertions: Set[Axiom], uncertainties: List[Uncertainty], prohibitedPrefixEquivalences: Set[String]): Task[List[Selection]] = {
    val whelk = Reasoner.assert(assertions, Map(NamespaceChecker.DelegateKey -> NamespaceChecker(prohibitedPrefixEquivalences, Nil)))
    if (isValid(whelk)) {
      val maxProbability = uncertainties.map(ah => Math.log(ah.mostProbable.probability)).sum
      resolveRemaining(List(Init(uncertainties, whelk))).map { selected =>
        println(s"Max probability: $maxProbability")
        val jointProbability = selected.map(s => Math.log(s.probability)).sum
        println(jointProbability)
        selected
      }
    } else ZIO.fail(BoomError("Given ontology is incoherent"))
  }

  @tailrec
  def resolveRemaining(selected: List[Selection]): Task[List[Selection]] =
    selected match {
      case Nil       => ZIO.fail(BoomError("Search must be called with at least an Init in previouslySelected"))
      case prev :: _ =>
        prev.remainingAmbiguities match {
          case Nil               => ZIO.succeed(selected)
          case next :: remaining => tryAdding(next, remaining, prev.reasonerState) match {
            case None            => searchForConflict(next, selected, remaining) match {
              case None              => ZIO.fail(BoomError("We aren't prepared for clumps that can't be added!"))
              case Some(nowSelected) => resolveRemaining(nowSelected)
            }
            case Some(selection) => resolveRemaining(selection :: selected)
          }
        }
    }

  private def searchForConflict(ambiguity: Ambiguity, selected: List[Selection], newRemaining: List[Ambiguity]): Option[List[Selection]] = {
    val InsertionPoint(index) = selected.bisect { selection =>
      ambiguity.sorted.exists { proposal =>
        val newReasonerState = Reasoner.assert(proposal.axioms, selection.reasonerState)
        !isValid(newReasonerState)
      }
    }
    //FIXME check for index edge cases
    val (newlyRemaining, stillSelectedPlusConflict) = selected.splitAt(index - 1)
    val conflict :: stillSelected = stillSelectedPlusConflict
    println(s"Making clump at $index")
    val perplexity = conflict match {
      case SelectedProposal(_, ag, _, _)                           => Perplexity(Set(ag)).add(ambiguity)
      case SelectedPerplexityProposal(_, c @ Perplexity(gs), _, _) => c.add(ambiguity)
      case Init(remainingAmbiguities, reasonerState)               =>
        println("We're at the beginning, what do we do??")
        ???
    }
    println(perplexity)
    val newlyRemainingAmbiguities = newlyRemaining.map {
      case SelectedProposal(_, ag, _, _)          => ag
      case SelectedPerplexityProposal(_, c, _, _) => c
    }
    val updatedRemaining = newlyRemainingAmbiguities.reverse ::: newRemaining
    tryAdding(perplexity, updatedRemaining, stillSelected.head.reasonerState).map(_ :: stillSelected)
  }

  private def tryAdding(possibility: Ambiguity, remaining: List[Ambiguity], reasonerState: ReasonerState): Option[Selection] = {
    possibility match {
      case ag: Uncertainty =>
        val maybeAdded = ag.sorted.to(LazyList).map { proposal =>
          val newReasonerState = Reasoner.assert(proposal.axioms, reasonerState)
          proposal -> newReasonerState
        }.find { case (_, state) => isValid(state) }
        maybeAdded.map {
          case (proposal, state) => SelectedProposal(proposal, ag, remaining, state)
        }

      case perplexity: Perplexity =>
        val maybeAdded = perplexity.sorted.to(LazyList).map { proposal =>
          val newReasonerState = Reasoner.assert(proposal.axioms, reasonerState)
          proposal -> newReasonerState
        }.find { case (_, state) => isValid(state) }
        maybeAdded.map {
          case (proposal, state) => SelectedPerplexityProposal(proposal, perplexity, remaining, state)
        }
    }
  }

  private def jointProbability(selections: List[SelectedProposal]): Double = selections.map(s => Math.log(s.selected.probability)).sum

  private def isValid(state: ReasonerState): Boolean = !isIncoherent(state) && !hasNamespaceViolations(state)

  private def isIncoherent(state: ReasonerState): Boolean =
    state.closureSubsBySuperclass(Bottom).exists(t => !t.isAnonymous && t != Bottom)

  private def hasNamespaceViolations(state: ReasonerState): Boolean =
    state.queueDelegates(NamespaceChecker.DelegateKey).asInstanceOf[NamespaceChecker].violations.nonEmpty


  private def unsatisfiableClasses(state: ReasonerState): Set[AtomicConcept] =
    state.closureSubsBySuperclass(Bottom).collect { case term: AtomicConcept if term != Bottom => term }

  final case class BoomError(message: String) extends Throwable(message)

}
