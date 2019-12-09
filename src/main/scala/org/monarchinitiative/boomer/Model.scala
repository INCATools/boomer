package org.monarchinitiative.boomer

import org.geneontology.whelk.{ConceptInclusion, ReasonerState}

object Model {

  sealed trait GeneralizedProposal {

    def axioms: Set[ConceptInclusion]

  }

  final case class Proposal(label: String, axioms: Set[ConceptInclusion], probability: Double) extends GeneralizedProposal

  sealed trait Ambiguity {

    def sorted: List[GeneralizedProposal]

  }

  final case class Uncertainty(proposals: Set[Proposal]) extends Ambiguity {

    val mostProbable: Proposal = proposals.maxBy(_.probability)

    val sorted: List[Proposal] = proposals.toList.sortBy(_.probability)(Ordering[Double].reverse)

  }

  final case class PerplexityProposal(proposal: Map[Uncertainty, Proposal]) extends GeneralizedProposal {

    def axioms: Set[ConceptInclusion] = proposal.values.flatMap(_.axioms).toSet

  }

  final case class Perplexity(uncertainties: Set[Uncertainty]) extends Ambiguity {

    //TODO this may grow too large
    def sorted: List[PerplexityProposal] =
      uncertainties.foldLeft(List(Map.empty[Uncertainty, Proposal])) { case (acc, uncertainty) =>
        uncertainty.proposals.toList.flatMap(p => acc.map(m => m.updated(uncertainty, p)))
      }.sortBy(_.values.map(_.probability).product)(Ordering[Double].reverse).map(PerplexityProposal)

    def add(possibility: Ambiguity): Perplexity = possibility match {
      case ag: Uncertainty => Perplexity(uncertainties + ag)
      case Perplexity(gs)  => Perplexity(uncertainties ++ gs)
    }

  }

  sealed trait Selection {

    def remainingAmbiguities: List[Ambiguity]

    def reasonerState: ReasonerState

    def probability: Double

  }

  final case class Init(remainingAmbiguities: List[Uncertainty], reasonerState: ReasonerState) extends Selection {

    override val probability: Double = 1.0

  }

  final case class SelectedProposal(selected: Proposal, uncertainty: Uncertainty, remainingAmbiguities: List[Ambiguity], reasonerState: ReasonerState) extends Selection {

    override def probability: Double = selected.probability

  }

  final case class SelectedPerplexityProposal(selected: PerplexityProposal, clump: Perplexity, remainingAmbiguities: List[Ambiguity], reasonerState: ReasonerState) extends Selection {

    override def probability: Double = selected.proposal.values.map(_.probability).product

  }

}
