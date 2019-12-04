package org.monarchinitiative.boomer

import org.geneontology.whelk.{ConceptInclusion, ReasonerState}

object Model {

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

}
