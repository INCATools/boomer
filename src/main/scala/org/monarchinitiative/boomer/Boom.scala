package org.monarchinitiative.boomer

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.{AtomicConcept, Axiom, Reasoner, ReasonerState}
import org.monarchinitiative.boomer.Model._
import org.monarchinitiative.boomer.Util.BisectSearchOp
import zio._
import zio.random.Random

import scala.Ordering.Double.TotalOrdering
import scala.annotation.tailrec
import scala.collection.Searching.InsertionPoint

object Boom {

  def evaluate(assertions: Set[Axiom],
               uncertainties: Set[Uncertainty],
               prohibitedPrefixEquivalences: Set[String],
               windowCount: Int,
               runs: Int): ZIO[Random, BoomError, List[Map[Uncertainty, (Proposal, Boolean)]]] = {
    val whelk = Reasoner.assert(assertions, Map(NamespaceChecker.DelegateKey -> NamespaceChecker(prohibitedPrefixEquivalences, Nil)))
    val binnedUncertainties = Util
      .groupByValueWindows(uncertainties.toList, windowCount, (u: Uncertainty) => u.mostProbable.probability)
      .filter(_.nonEmpty)
      .reverse
    binnedUncertainties.foreach { us =>
      scribe.info(s"Bin size: ${us.size}; Most probable: ${us.map(_.mostProbable.probability).max}")
    }
    val maxProbability = uncertainties.toList.map(ah => Math.log(ah.mostProbable.probability)).sum
    scribe.info(s"Max possible joint probability: $maxProbability")
    val oneEvaluation = for {
      orderedUncertainties <- shuffleWithinWindows(binnedUncertainties)
      selections <- evaluateInOrder(whelk, orderedUncertainties, prohibitedPrefixEquivalences)
    } yield selections.flatMap(collectChoices).toMap
    ZIO.collectAllPar(List.fill(runs)(oneEvaluation))
  }

  private def shuffleWithinWindows(windows: List[List[Uncertainty]]): ZIO[Random, Nothing, List[Uncertainty]] =
    ZIO.foreach(windows)(w => random.shuffle(w)).map(_.flatten)

  def evaluateInOrder(initialState: ReasonerState,
                      uncertainties: List[Uncertainty],
                      prohibitedPrefixEquivalences: Set[String]): IO[BoomError, List[Selection]] =
    if (isValid(initialState))
      resolve(uncertainties, initialState).map { selected =>
        val jointProbability = selected.map(s => Math.log(s.probability)).sum
        scribe.info(s"Found joint probability: $jointProbability")
        selected
      }
    else if (isIncoherent(initialState)) ZIO.fail(BoomErrorMessage("Given ontology is incoherent"))
    else {
      val violations = getNamespaceViolations(initialState)
        .map { case (AtomicConcept(left), AtomicConcept(right)) =>
          s"$left EquivalentTo $right"
        }
        .mkString("\n")
      ZIO.fail(BoomErrorMessage(s"Given ontology has equivalents within namespace:\n$violations"))
    }

  def organizeResults(results: List[Map[Uncertainty, (Proposal, Boolean)]])
    : (Map[Uncertainty, (Proposal, Boolean)], Map[Uncertainty, Map[(Proposal, Boolean), Int]]) = {
    val mostProbableResult = results.maxBy(_.map(_._2._1.probability).sum)
    // Count, for each uncertainty, how often each proposal was found
    val counted = results.flatMap(_.toList).groupMap(_._1)(_._2).map { case (uncertainty, selections) =>
      uncertainty -> selections.groupMap(identity)(_ => 1).view.mapValues(_.sum).toMap
    }
    (mostProbableResult, counted)
  }

  def collectChoices(selection: Selection): Set[(Uncertainty, (Proposal, Boolean))] = selection match {
    case SelectedProposal(proposal, uncertainty, _, _, _) => Set(uncertainty -> (proposal, proposal == uncertainty.mostProbable))
    case SelectedPerplexityProposal(selected, _, _, _, _) =>
      selected.proposal.to(Set).map { case (uncertainty, proposal) => uncertainty -> (proposal, proposal == uncertainty.mostProbable) }
  }

  def resolve(uncertainties: List[Uncertainty], initialReasonerState: ReasonerState): IO[BoomError, List[Selection]] =
    uncertainties match {
      case Nil => ZIO.fail(BoomErrorMessage("No uncertainties to resolve."))
      case next :: remaining =>
        tryAdding(next, remaining, initialReasonerState) match {
          case None            => ZIO.fail(BoomErrorMessage("The first uncertainty has no proposals which are compatible with the initial reasoner state."))
          case Some(selection) => resolveRemaining(selection :: Nil)
        }
    }

  @tailrec
  def resolveRemaining(selected: List[Selection]): IO[BoomError, List[Selection]] =
    selected match {
      case Nil => ZIO.fail(BoomErrorMessage("Search must be called with at least one added selection"))
      case prev :: _ =>
        prev.remainingAmbiguities match {
          case Nil => ZIO.succeed(selected)
          case next :: remaining =>
            tryAdding(next, remaining, prev.reasonerState) match {
              case None =>
                searchForConflict(next, selected, remaining) match {
                  case None              => ZIO.fail(UnresolvableUncertainties)
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
    val (newlyRemaining, stillSelectedPlusConflict) = selected.splitAt(index - 1)
    stillSelectedPlusConflict match {
      case Nil => None
      case conflict :: stillSelected =>
        scribe.debug(s"Making perplexity at $index")
        val newPerplexity = conflict match {
          case SelectedProposal(_, uncertainty, _, _, _)          => Perplexity(Set(uncertainty)).add(ambiguity)
          case SelectedPerplexityProposal(_, perplexity, _, _, _) => perplexity.add(ambiguity)
        }
        scribe.debug(newPerplexity.toString)
        val newlyRemainingAmbiguities = newlyRemaining.map {
          case SelectedProposal(_, uncertainty, _, _, _)          => uncertainty
          case SelectedPerplexityProposal(_, perplexity, _, _, _) => perplexity
        }
        val updatedRemaining = newlyRemainingAmbiguities.reverse ::: newRemaining
        tryAdding(newPerplexity, updatedRemaining, conflict.previousReasonerState).map(_ :: stillSelected)
    }
  }

  private def tryAdding(possibility: Ambiguity, remaining: List[Ambiguity], reasonerState: ReasonerState): Option[Selection] =
    possibility match {
      case ag: Uncertainty =>
        val maybeAdded = ag.sorted
          .to(LazyList)
          .map { proposal =>
            val newReasonerState = Reasoner.assert(proposal.axioms, reasonerState)
            proposal -> newReasonerState
          }
          .find { case (_, state) => isValid(state) }
        maybeAdded.map { case (proposal, newState) =>
          SelectedProposal(proposal, ag, remaining, newState, reasonerState)
        }
      case perplexity: Perplexity =>
        val maybeAdded = perplexity.sorted
          .to(LazyList)
          .map { proposal =>
            val newReasonerState = Reasoner.assert(proposal.axioms, reasonerState)
            proposal -> newReasonerState
          }
          .find { case (_, state) => isValid(state) }
        maybeAdded.map { case (proposal, newState) =>
          SelectedPerplexityProposal(proposal, perplexity, remaining, newState, reasonerState)
        }
    }

  private def jointProbability(selections: List[SelectedProposal]): Double = selections.map(s => Math.log(s.selected.probability)).sum

  private def isValid(state: ReasonerState): Boolean = !isIncoherent(state) && !hasNamespaceViolations(state)

  private def isIncoherent(state: ReasonerState): Boolean =
    state.closureSubsBySuperclass(Bottom).exists(t => !t.isAnonymous && t != Bottom)

  private def hasNamespaceViolations(state: ReasonerState): Boolean = getNamespaceViolations(state).nonEmpty

  private def getNamespaceViolations(state: ReasonerState): List[(AtomicConcept, AtomicConcept)] =
    state.queueDelegates(NamespaceChecker.DelegateKey).asInstanceOf[NamespaceChecker].violations

  private def unsatisfiableClasses(state: ReasonerState): Set[AtomicConcept] =
    state.closureSubsBySuperclass(Bottom).collect { case term: AtomicConcept if term != Bottom => term }

  sealed abstract class BoomError(val message: String) extends Throwable(message)

  object BoomError {

    def unapply(error: BoomError): Option[String] = Some(error.message)

  }

  final case class BoomErrorMessage(msg: String) extends BoomError(msg)

  case object UnresolvableUncertainties extends BoomError("No configuration of the uncertainties can be added to the ontology")

}
