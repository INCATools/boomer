package org.monarchinitiative.boomer

import jp.yukoba.collection.immutable.PriorityQueue
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

  type ResolvedUncertainty = (Uncertainty, (Proposal, Boolean))
  type ResolvedUncertainties = Map[Uncertainty, (Proposal, Boolean)]

  object ResolvedUncertainties {

    def empty: ResolvedUncertainties = Map.empty[Uncertainty, (Proposal, Boolean)]

  }

  def evaluate(assertions: Set[Axiom],
               uncertainties: Set[Uncertainty],
               prohibitedPrefixEquivalences: Set[String],
               initialReasonerState: ReasonerState,
               windowCount: Int,
               runs: Int,
               exhaustive: Boolean): ZIO[Random, BoomError, List[ResolvedUncertainties]] = {
    val binnedUncertainties = Util
      .groupByValueWindows(uncertainties.toList, windowCount, (u: Uncertainty) => u.mostProbable.probability)
      .filter(_.nonEmpty)
      .reverse
    binnedUncertainties.foreach { us =>
      scribe.info(s"Bin size: ${us.size}; Most probable: ${us.map(_.mostProbable.probability).max}")
    }
    val maxProbability = uncertainties.toList.map(ah => Math.log(ah.mostProbable.probability)).sum
    scribe.info(s"Max possible joint probability: $maxProbability")
    val reasonerWithNamespaceChecker = initialReasonerState.copy(queueDelegates =
      initialReasonerState.queueDelegates + (NamespaceChecker.DelegateKey -> NamespaceChecker(prohibitedPrefixEquivalences, Nil)))
    val oneEvaluation = for {
      orderedUncertainties <- shuffleWithinWindows(binnedUncertainties)
      selections <- evaluateInOrder(reasonerWithNamespaceChecker, orderedUncertainties, prohibitedPrefixEquivalences, exhaustive)
    } yield selections.flatMap(collectChoices).toMap
    ZIO.collectAllPar(List.fill(runs)(oneEvaluation))
  }

  private def shuffleWithinWindows(windows: List[List[Uncertainty]]): ZIO[Random, Nothing, List[Uncertainty]] =
    ZIO.foreach(windows)(w => random.shuffle(w)).map(_.flatten)

  def evaluateInOrder(initialState: ReasonerState,
                      uncertainties: List[Uncertainty],
                      prohibitedPrefixEquivalences: Set[String],
                      exhaustive: Boolean): IO[BoomError, List[Selection]] =
    if (!isIncoherent(initialState))
      resolve(uncertainties, initialState, exhaustive).map { selected =>
        val jointProbability = selected.map(s => Math.log(s.probability)).sum
        scribe.info(s"Found joint probability: $jointProbability")
        selected
      }
    else ZIO.fail(BoomErrorMessage("Given ontology is incoherent"))

  def organizeResults(results: List[ResolvedUncertainties]): (ResolvedUncertainties, Map[Uncertainty, Map[(Proposal, Boolean), Int]]) = {
    val mostProbableResult = results.maxBy(jointProbability)
    // Count, for each uncertainty, how often each proposal was found
    val uncertaintyResolutionsInAllRuns = results.flatMap(_.toList)
    val counted = uncertaintyResolutionsInAllRuns.groupMap(_._1)(_._2).map { case (uncertainty, selections) =>
      uncertainty -> selections.groupBy(identity).view.mapValues(_.size).to(Map)
    }
    (mostProbableResult, counted)
  }

  def collectChoices(selection: Selection): Set[ResolvedUncertainty] = selection match {
    case SelectedProposal(proposal, uncertainty, _, _, _) => Set(uncertainty -> (proposal, proposal == uncertainty.mostProbable))
    case SelectedPerplexityProposal(selected, _, _, _, _) =>
      selected.proposal.to(Set).map { case (uncertainty, proposal) => uncertainty -> (proposal, proposal == uncertainty.mostProbable) }
  }

  def resolve(uncertainties: List[Uncertainty], initialReasonerState: ReasonerState, exhaustive: Boolean): IO[BoomError, List[Selection]] =
    uncertainties match {
      case Nil => ZIO.fail(BoomErrorMessage("No uncertainties to resolve."))
      case next :: remaining =>
        val result = if (exhaustive) {
          tryAdding(Perplexity(uncertainties.to(Set)), Nil, initialReasonerState)
        } else {
          tryAdding(next, remaining, initialReasonerState)
        }
        result match {
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
        resolvePerplexity(perplexity, reasonerState) match {
          case Right(Resolved(proposals, newState)) =>
            val selected = PerplexityProposal(proposals.map(res => res.uncertainty -> res.proposal).to(Map))
            Some(SelectedPerplexityProposal(selected, perplexity, remaining, newState, reasonerState))
          case Left(error) =>
            scribe.error(error.message)
            None //FIXME use error
        }
    }

  sealed private trait DwindleStatus

  final private case class Open(dus: List[DwindlingUncertainty]) extends DwindleStatus

  final private case class Resolved(proposals: List[Resolution], reasonerState: ReasonerState) extends DwindleStatus

  private object Closed extends DwindleStatus

  private def resolvePerplexity(perplexity: Perplexity, reasonerState: ReasonerState): Either[BoomError, Resolved] = {
    val uncertainties = perplexity.uncertainties.to(List).sortBy(_.mostProbable.probability)(Ordering[Double].reverse)
    uncertainties match {
      case first :: rest =>
        val dus = first.proposals.to(List).map(p => DwindlingUncertainty(first, p, Nil, reasonerState, rest))
        val queue = PriorityQueue(dus: _*)
        processQueue(queue) match {
          case Closed        => Left(BoomErrorMessage("No possible resolution of perplexity"))
          case res: Resolved => Right(res)
          case _: Open       => Left(BoomErrorMessage("Perplexity resolution terminated with unresolved uncertainty"))
        }
      case Nil => Left(BoomErrorMessage("A perplexity was created without any uncertainties"))
    }
  }

  @tailrec
  private def processQueue(queue: PriorityQueue[DwindlingUncertainty]): DwindleStatus =
    queue.dequeueOption match {
      case Some((next, remaining)) =>
        dwindle(next) match {
          case Open(dus)     => processQueue(remaining.enqueue(dus))
          case Closed        => processQueue(remaining)
          case res: Resolved => res
        }
      case None => Closed
    }

  private def dwindle(du: DwindlingUncertainty): DwindleStatus = {
    val newReasonerState = Reasoner.assert(du.proposal.axioms, du.previousReasonerState)
    if (isValid(newReasonerState)) {
      val newSelected = Resolution(du.uncertainty, du.proposal) :: du.selected
      du.remaining match {
        case next :: rest =>
          val dus = next.proposals.to(List).map(p => DwindlingUncertainty(next, p, newSelected, newReasonerState, rest))
          Open(dus)
        case Nil => Resolved(newSelected, newReasonerState)
      }
    } else {
//      val one: BigInt = 1
//      val eliminated = du.remaining.map(_.proposals.size).foldLeft(one)(_ * _)
//      scribe.info(s"Eliminated choices: $eliminated")
      Closed
    }
  }

  private def jointProbability(selections: List[SelectedProposal]): Double = selections.map(s => Math.log(s.selected.probability)).sum

  def jointProbability(result: ResolvedUncertainties): Double =
    result.map { case (_, (proposal, _)) => Math.log(proposal.probability) }.sum

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
