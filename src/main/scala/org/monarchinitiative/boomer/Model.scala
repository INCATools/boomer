package org.monarchinitiative.boomer

import org.geneontology.whelk.{Axiom, ConceptInclusion, ReasonerState}

import scala.Ordering.Double.TotalOrdering

object Model {

  sealed trait GeneralizedProposal {

    def axioms: Set[ConceptInclusion]

  }

  /** A set of axioms representing a possible state of the world. All axioms
    * within the proposal are considered together as a unit.
    *
    * @param label       a descriptive label for the condition represented by this group of axioms
    * @param axioms      the group of axioms constituting this proposal (restricted to concept inclusions)
    * @param probability the prior probability asserted for the truthfulness of this proposal
    */
  final case class Proposal(label: AnyRef, axioms: Set[ConceptInclusion], probability: Double) extends GeneralizedProposal

  /** A group of hypothetical proposals, either a single set of alternative states (Uncertainty),
    * or a combinatorial set of states representing multiple Uncertainties (Perplexity)
    */
  sealed trait Ambiguity {

    /** Proposals to attempt, in decreasing order of prior probability
      */
    def sorted: Iterable[GeneralizedProposal]

  }

  /** A set of proposals (axiom sets) which represent alternative possibilities. One
    * proposal must be selected to resolve the uncertainty.
    */
  final case class Uncertainty(proposals: Set[Proposal]) extends Ambiguity {

    val mostProbable: Proposal = proposals.maxBy(_.probability)

    override val sorted: List[Proposal] = proposals.toList.sortBy(_.probability)(Ordering[Double].reverse)

    override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  }

  /** A joint proposal consisting of one proposal from each uncertainty within a Perplexity.
    */
  final case class PerplexityProposal(proposal: Map[Uncertainty, Proposal]) extends GeneralizedProposal {

    /** The set union of all axioms from the group of proposals.
      */
    override def axioms: Set[ConceptInclusion] = proposal.values.flatMap(_.axioms).to(Set)

  }

  /** A group of uncertainties whose states are evaluated combinatorially, increasing the likelihood
    * that a mutually compatible configuration of these uncertainties will be found.
    */
  final case class Perplexity(uncertainties: Set[Uncertainty]) extends Ambiguity {

    /** All possible combinations of a selection from each Uncertainty within this Perplexity.
      * Proposals are returned in order of decreasing joint probability.
      * The size of this list will grow exponentially with the number of uncertainties.
      */
    def sorted: LazyList[PerplexityProposal] = {
      val sortedUncertainties = uncertainties.to(List).sortBy(_.mostProbable.probability)(Ordering[Double].reverse)
      val lazySorted = sortedUncertainties.to(LazyList)
      def loop(theseUncertainties: LazyList[Uncertainty]): LazyList[Map[Uncertainty, Proposal]] =
        theseUncertainties match {
          case firstUncertainty #:: restUncertainties =>
            for {
              proposal <- firstUncertainty.sorted.to(LazyList)
              moreProposals <- loop(restUncertainties)
            } yield moreProposals + (firstUncertainty -> proposal)
          case empty => LazyList(Map.empty[Uncertainty, Proposal])
        }
      loop(lazySorted).map(proposals => PerplexityProposal(proposals))
    }

    /** Create a new Perplexity combining the uncertainties within the given Ambiguity
      * (a single Uncertainty or multiple in a Perplexity) with the uncertainties in this Perplexity.
      */
    def add(ambiguity: Ambiguity): Perplexity = ambiguity match {
      case ag: Uncertainty => Perplexity(uncertainties + ag)
      case Perplexity(gs)  => Perplexity(uncertainties ++ gs)
    }

  }

  /** An ambiguity which has had a proposal successfully added to a reasoner state.
    */
  sealed trait Selection {

    /** Ambiguities remaining to evaluate after adding this selection.
      */
    def remainingAmbiguities: List[Ambiguity]

    /** Reasoner state resulting from adding this selection.
      */
    def reasonerState: ReasonerState

    /** Reasoner state prior to adding this selection.
      */
    def previousReasonerState: ReasonerState

    /** Probability associated with choosing this selection (possibly a joint probability of contained ambiguities).
      */
    def probability: Double

  }

  final case class SelectedProposal(selected: Proposal,
                                    uncertainty: Uncertainty,
                                    remainingAmbiguities: List[Ambiguity],
                                    reasonerState: ReasonerState,
                                    previousReasonerState: ReasonerState)
      extends Selection {

    override def probability: Double = selected.probability

  }

  final case class SelectedPerplexityProposal(selected: PerplexityProposal,
                                              clump: Perplexity,
                                              remainingAmbiguities: List[Ambiguity],
                                              reasonerState: ReasonerState,
                                              previousReasonerState: ReasonerState)
      extends Selection {

    override def probability: Double = selected.proposal.values.map(_.probability).product

  }

  final case class ProbabilisticOntology(axioms: Set[Axiom], uncertainties: Set[Uncertainty])

  object ProbabilisticOntology {

    val empty: ProbabilisticOntology = ProbabilisticOntology(Set.empty, Set.empty)

  }

  final case class Resolution(uncertainty: Uncertainty, proposal: Proposal)

  final case class DwindlingUncertainty(uncertainty: Uncertainty,
                                        proposal: Proposal,
                                        selected: List[Resolution],
                                        previousReasonerState: ReasonerState,
                                        remaining: List[Uncertainty]) {

    /** Log joint probability resulting from this proposal and
      * all subsequent highest probability choices.
      */
    val bestCaseScenario: Double = Math.log(proposal.probability) +
      remaining.map(u => Math.log(u.mostProbable.probability)).sum +
      selected.map(p => Math.log(p.proposal.probability)).sum

  }

  object DwindlingUncertainty {

    implicit val ordering: Ordering[DwindlingUncertainty] = new Ordering[DwindlingUncertainty]() {

      override def compare(x: DwindlingUncertainty, y: DwindlingUncertainty): Int =
        Ordering[Double].compare(x.bestCaseScenario, y.bestCaseScenario)

    }

  }

}
