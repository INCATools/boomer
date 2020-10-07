package org.monarchinitiative.boomer

import org.geneontology.whelk.{Axiom, ConceptInclusion, ReasonerState}

import scala.Ordering.Double.TotalOrdering

object Model {

  sealed trait GeneralizedProposal {

    def axioms: Set[ConceptInclusion]

  }

  /**
    * A set of axioms representing a possible state of the world. All axioms
    * within the proposal are considered together as a unit.
    *
    * @param label       a descriptive label for the condition represented by this group of axioms
    * @param axioms      the group of axioms constituting this proposal (restricted to concept inclusions)
    * @param probability the prior probability asserted for the truthfulness of this proposal
    */
  final case class Proposal(label: String, axioms: Set[ConceptInclusion], probability: Double) extends GeneralizedProposal

  /**
    * A group of hypothetical proposals, either a single set of alternative states (Uncertainty),
    * or a combinatorial set of states representing multiple Uncertainties (Perplexity)
    */
  sealed trait Ambiguity {

    /**
      * Proposals to attempt, in decreasing order of prior probability
      */
    def sorted: List[GeneralizedProposal]

  }

  /**
    * A set of proposals (axiom sets) which represent alternative possibilities. One
    * proposal must be selected to resolve the uncertainty.
    */
  final case class Uncertainty(proposals: Set[Proposal]) extends Ambiguity {

    val mostProbable: Proposal = proposals.maxBy(_.probability)

    override val sorted: List[Proposal] = proposals.toList.sortBy(_.probability)(Ordering[Double].reverse)

  }

  /**
    * A joint proposal consisting of one proposal from each uncertainty within a Perplexity.
    */
  final case class PerplexityProposal(proposal: Map[Uncertainty, Proposal]) extends GeneralizedProposal {

    /**
      * The set union of all axioms from the group of proposals.
      */
    override def axioms: Set[ConceptInclusion] = proposal.values.flatMap(_.axioms).to(Set)

  }

  /**
    * A group of uncertainties whose states are evaluated combinatorially, increasing the likelihood
    * that a mutually compatible configuration of these uncertaintites will be found.
    */
  final case class Perplexity(uncertainties: Set[Uncertainty]) extends Ambiguity {

    /**
      * All possible combinations of a selection from each Uncertainty within this Perplexity.
      * Proposals are returning in order of decreasing joint probability.
      * The size of this list will grow exponentially with the number of uncertainties.
      */
    def sorted: List[PerplexityProposal] =
      uncertainties
        .foldLeft(List(Map.empty[Uncertainty, Proposal])) { case (acc, uncertainty) =>
          uncertainty.proposals.toList.flatMap(p => acc.map(m => m.updated(uncertainty, p)))
        }
        .sortBy(_.values.map(_.probability).product)(Ordering[Double].reverse)
        .map(PerplexityProposal)

    /**
      * Create a new Perplexity combining the uncertainties within the given Ambiguity
      * (a single Uncertainty or multiple in a Perplexity) with the uncertainties in this Perplexity.
      */
    def add(ambiguity: Ambiguity): Perplexity = ambiguity match {
      case ag: Uncertainty => Perplexity(uncertainties + ag)
      case Perplexity(gs)  => Perplexity(uncertainties ++ gs)
    }

  }

  /**
    * An ambiguity which has had a proposal successfully added to a reasoner state.
    */
  sealed trait Selection {

    /**
      * Ambiguities remaining to evaluate after adding this selection.
      */
    def remainingAmbiguities: List[Ambiguity]

    /**
      * Reasoner state resulting from adding this selection.
      */
    def reasonerState: ReasonerState

    /**
      * Reasoner state prior to adding this selection.
      */
    def previousReasonerState: ReasonerState

    /**
      * Probability associated with choosing this selection (possibily a joint probability of contained ambiguities).
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

}
