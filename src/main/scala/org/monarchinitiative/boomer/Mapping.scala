package org.monarchinitiative.boomer

import org.geneontology.whelk._
import org.monarchinitiative.boomer.Boom.{BoomErrorMessage, ResolvedUncertainties}
import org.monarchinitiative.boomer.Model.{Proposal, Uncertainty}
import org.monarchinitiative.boomer.OntUtil.{disjointSibling, expandCURIE}
import zio.blocking.Blocking
import zio.{Task, ZIO}

import java.io.File
import scala.io.Source

object Mapping {

  def readPTable(file: File, prefixes: Map[String, String]): ZIO[Blocking, Throwable, Set[Mapping]] =
    Task
      .effect(Source.fromFile(file, "utf-8"))
      .bracketAuto { source =>
        ZIO.foreach(source.getLines().to(Iterable))(parsePTableLine(_, prefixes))
      }
      .map(_.to(Set))

  def parsePTableLine(line: String, prefixes: Map[String, String]): Task[Mapping] = {
    val columns = line.split("\\t", -1)
    if (columns.size == 6) {
      val leftCURIE = columns(0).trim
      val rightCURIE = columns(1).trim
      for {
        leftID <- ZIO.fromOption(expandCURIE(leftCURIE, prefixes)).orElseFail(BoomErrorMessage(s"Failed expanding CURIE: $leftCURIE"))
        rightID <- ZIO.fromOption(expandCURIE(rightCURIE, prefixes)).orElseFail(BoomErrorMessage(s"Failed expanding CURIE: $rightCURIE"))
        left = AtomicConcept(leftID)
        right = AtomicConcept(rightID)
        probProperSubLeftRight <- Task.effect(columns(2).trim.toDouble)
        probProperSubRightLeft <- Task.effect(columns(3).trim.toDouble)
        probEquivalent <- Task.effect(columns(4).trim.toDouble)
        probNoSubsumption <- Task.effect(columns(5).trim.toDouble)
        disjointSiblingOfLeftUnderRight = disjointSibling(left, right)
        disjointSiblingOfRightUnderLeft = disjointSibling(right, left)
      } yield {
        val equivalenceProposal =
          Proposal(Equivalent(left, right), Set(ConceptInclusion(left, right), ConceptInclusion(right, left)), probEquivalent)
        val siblingsProposal = Proposal(Siblings(left, right), disjointSiblingOfLeftUnderRight ++ disjointSiblingOfRightUnderLeft, probNoSubsumption)
        Mapping(
          left,
          right,
          Uncertainty(
            Set(
              Proposal(ProperSubClassOf(left, right), disjointSiblingOfLeftUnderRight + ConceptInclusion(left, right), probProperSubLeftRight),
              Proposal(ProperSuperClassOf(left, right), disjointSiblingOfRightUnderLeft + ConceptInclusion(right, left), probProperSubRightLeft),
              equivalenceProposal,
              siblingsProposal
            ).filter(_.probability > 0.0)
          ),
          equivalenceProposal
        )
      }
    } else Task.fail(BoomErrorMessage(s"Invalid ptable line: $line"))
  }

  def makeMaximalEquivalenceCliques(mappings: Set[Mapping], assertions: Set[Axiom]): Map[AtomicConcept, Set[AtomicConcept]] = {
    val allAxioms: Set[Axiom] = mappings.flatMap(_.equivalenceProposal.axioms) //assertions ++
    val mappingsWhelk = Reasoner.assert(axioms = allAxioms, disableBottom = true)
    val mappingsTaxonomy = mappingsWhelk.computeTaxonomy
    val mappingsGroups = mappingsTaxonomy.map { case (concept, (equivs, _)) => equivs + concept }.to(Set)
    val termToRepresentative = (for {
      group <- mappingsGroups.iterator
      representative = group.minBy(_.id)
      term <- group
    } yield term -> representative).toMap
    val representativeToGroup = mappingsGroups.iterator.map(g => g.toSeq.minBy(_.id) -> g).toMap
    def replaceInConcept(concept: Concept, map: Map[AtomicConcept, AtomicConcept]): Concept =
      concept match {
        case a: AtomicConcept                      => map.getOrElse(a, a)
        case Conjunction(left, right)              => Conjunction(replaceInConcept(left, map), replaceInConcept(right, map))
        case Disjunction(operands)                 => Disjunction(operands.map(o => replaceInConcept(o, map)))
        case ExistentialRestriction(role, concept) => ExistentialRestriction(role, replaceInConcept(concept, map))
        case sr: SelfRestriction                   => sr
        case dr: DataRestriction                   => dr
        case dhv: DataHasValue                     => dhv
        case Complement(concept)                   => Complement(replaceInConcept(concept, map))
        case n: Nominal                            => n
      }
    def replaceInRuleAtom(atom: RuleAtom, map: Map[AtomicConcept, AtomicConcept]): RuleAtom =
      atom match {
        case ConceptAtom(predicate, argument) => ConceptAtom(replaceInConcept(predicate, map), argument)
        case ra: RoleAtom                     => ra
      }
    val updatedAxioms: Set[Axiom] = assertions.map {
      case ConceptInclusion(subclass, superclass) =>
        ConceptInclusion(replaceInConcept(subclass, termToRepresentative), replaceInConcept(superclass, termToRepresentative))
      case ri: RoleInclusion                     => ri
      case rc: RoleComposition                   => rc
      case ConceptAssertion(concept, individual) => ConceptAssertion(replaceInConcept(concept, termToRepresentative), individual)
      case ra: RoleAssertion                     => ra
      case Rule(body, head) =>
        Rule(body.map(a => replaceInRuleAtom(a, termToRepresentative)), head.map(a => replaceInRuleAtom(a, termToRepresentative)))
    }
    val whelk = Reasoner.assert(updatedAxioms, disableBottom = true)
    val taxonomy = whelk.computeTaxonomy
    val equivGroups = taxonomy.map { case (concept, (equivs, _)) => equivs + concept }.to(Set)
    val representativeToEquivGroup = (for {
      group <- equivGroups.iterator
      representative = group.minBy(_.id)
    } yield representative -> group).toMap
    val representativesToFullCliques = for {
      (representative, equivGroup) <- representativeToEquivGroup
      groupMembersForAllEquivs = equivGroup.flatMap(equiv => representativeToGroup.get(equiv).toSet.flatten)
    } yield representative -> (equivGroup ++ groupMembersForAllEquivs)
    (for {
      clique <- representativesToFullCliques.valuesIterator
      c <- clique
    } yield c -> clique).toMap
  }

  def groupHotspotsByEquivalenceClique(hotspots: Map[Uncertainty, Map[(Proposal, Boolean), Int]],
                                       equivalenceCliques: Map[AtomicConcept, Set[AtomicConcept]],
                                       mappings: Set[Mapping]): Map[Option[Set[AtomicConcept]], Map[Uncertainty, Map[(Proposal, Boolean), Int]]] = {
    val mappingsByUncertainty = mappings.map(mapping => mapping.uncertainty -> mapping).toMap
    val res = hotspots.groupBy { case (uncertainty, _) =>
      for {
        mapping <- mappingsByUncertainty.get(uncertainty)
        clique <- equivalenceCliques.get(mapping.left)
      } yield clique
    }
    res
  }

  def groupResultsByEquivalenceClique(results: List[List[ResolvedUncertainties]],
                                      equivalenceCliques: Map[AtomicConcept, Set[AtomicConcept]],
                                      mappings: Set[Mapping]): Map[Set[AtomicConcept], List[ResolvedUncertainties]] = {
    val mappingsByUncertainty = mappings.map(mapping => mapping.uncertainty -> mapping).toMap
    results
      .map { result =>
        val cliqueOption = for {
          (uncertainty, _) <- result.head.uncertainties.headOption
          mapping <- mappingsByUncertainty.get(uncertainty)
          clique <- equivalenceCliques.get(mapping.left)
        } yield clique
        cliqueOption.getOrElse(Set.empty) -> result
      }
      .to(Map)
  }

}

final case class Mapping(left: AtomicConcept, right: AtomicConcept, uncertainty: Uncertainty, equivalenceProposal: Proposal)

sealed trait MappingRelation {

  def left: AtomicConcept
  def right: AtomicConcept
  def label: String

}

final case class ProperSubClassOf(left: AtomicConcept, right: AtomicConcept) extends MappingRelation {
  def label = "ProperSubClassOf"
}

final case class ProperSuperClassOf(left: AtomicConcept, right: AtomicConcept) extends MappingRelation {
  def label = "ProperSuperClassOf"
}

final case class Equivalent(left: AtomicConcept, right: AtomicConcept) extends MappingRelation {
  def label = "EquivalentTo"
}

final case class Siblings(left: AtomicConcept, right: AtomicConcept) extends MappingRelation {
  def label = "SiblingOf"
}
