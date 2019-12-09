package org.monarchinitiative.boomer

import java.io.File
import java.util.UUID

import org.apache.commons.codec.digest.DigestUtils
import org.geneontology.whelk.BuiltIn.Bottom
import org.geneontology.whelk.{AtomicConcept, Axiom, Bridge, ConceptInclusion, Conjunction}
import org.monarchinitiative.boomer.Boom.BoomError
import org.monarchinitiative.boomer.Model.{AlternativesGroup, Proposal}
import org.openrdf.model.vocabulary.DCTERMS
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAnnotation, OWLAnnotationProperty, OWLAnnotationSubject, OWLAnnotationValue, OWLAnonymousIndividual, OWLAxiom, OWLIndividual, OWLNamedIndividual, OWLOntology, OWLSubClassOfAxiom}
import org.semanticweb.owlapi.model.parameters.Imports
import zio._
import zio.blocking._

import scala.io.Source
import scala.jdk.CollectionConverters._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.search.EntitySearcher
import org.semanticweb.owlapi.vocab.{DublinCoreVocabulary, OWLRDFVocabulary}

object OntUtil {

  val DisjointSiblingPrefix = "http://boom.monarchinitiative.org/vocab/disjoint_sibling#"
  val HasProbability = "http://semanticscience.org/resource/SIO_000638"
  private val HasProbabilityAP = AnnotationProperty(HasProbability)
  private val IsPartOfAP = AnnotationProperty(DCTERMS.IS_PART_OF.stringValue)
  private val AlternativesGroupClass = Class("") //FIXME
  private val ProposalClass = Class("") //FIXME

  def readPTable(file: File): ZIO[Blocking, Throwable, Set[AlternativesGroup]] = for {
    source <- Task.effect(Source.fromFile(file, "utf-8"))
    lines <- Task.effect(source.getLines())
    parsed <- Task.effect(lines.map(parsePTableLine).toList)
    entries <- ZIO.collectAll(parsed)
  } yield entries.toSet

  private def parsePTableLine(line: String): Task[AlternativesGroup] = {
    val columns = line.split("\\t", -1)
    if (columns.size == 6) {
      val leftCURIE = columns(0).trim
      val rightCURIE = columns(1).trim
      val left = AtomicConcept(parseCURIE(leftCURIE))
      val right = AtomicConcept(parseCURIE(rightCURIE))
      for {
        probProperSubLeftRight <- Task.effect(columns(2).trim.toDouble)
        probProperSubRightLeft <- Task.effect(columns(3).trim.toDouble)
        probEquivalent <- Task.effect(columns(4).trim.toDouble)
        probNoSubsumption <- Task.effect(columns(5).trim.toDouble)
        disjointSiblingOfLeftUnderRight = disjointSibling(left, right)
        disjointSiblingOfRightUnderLeft = disjointSibling(right, left)
      } yield {
        AlternativesGroup(Set(
          Proposal(s"$leftCURIE ProperSubClassOf $rightCURIE", disjointSiblingOfLeftUnderRight + ConceptInclusion(left, right), probProperSubLeftRight),
          Proposal(s"$leftCURIE ProperSuperClassOf $rightCURIE", disjointSiblingOfRightUnderLeft + ConceptInclusion(right, left), probProperSubRightLeft),
          Proposal(s"$leftCURIE EquivalentTo $rightCURIE", Set(ConceptInclusion(left, right), ConceptInclusion(right, left)), probEquivalent),
          Proposal(s"$leftCURIE SiblingOf $rightCURIE", disjointSiblingOfLeftUnderRight ++ disjointSiblingOfRightUnderLeft, probNoSubsumption)
        ).filter(_.probability > 0.0))
      }
    } else Task.fail(BoomError(s"Invalid ptable line: $line"))
  }

  final case class ProbabilisticOntology(axioms: Set[Axiom], hypotheticals: Set[AlternativesGroup])

  object ProbabilisticOntology {

    val empty = ProbabilisticOntology(Set.empty, Set.empty)

  }

  def readProbabilisticOntology(file: File): ZIO[Blocking, Throwable, ProbabilisticOntology] = for {
    manager <- Task.effect(OWLManager.createOWLOntologyManager())
    inFile <- Task.effect(IRI.create(file))
    ontology <- effectBlocking(manager.loadOntology(inFile))
    po <- partitionOntology(ontology)
  } yield po

  private def partitionOntology(ontology: OWLOntology): Task[ProbabilisticOntology] = {
    val groups = EntitySearcher.getInstances(AlternativesGroupClass, ontology).asScala.toSet[OWLIndividual].map(_ -> Set.empty[Proposal]).toMap
    val proposals: Task[(Map[OWLIndividual, Set[Proposal]], Set[OWLAxiom])] =
      EntitySearcher.getInstances(ProposalClass, ontology).asScala.toSet.foldLeft(Task.effect(groups -> Set.empty[OWLAxiom])) {
        case (acc, ind) =>
          val proposalAnnotationSubject = ind match {
            case anon: OWLAnonymousIndividual => anon
            case named: OWLNamedIndividual    => named.getIRI
          }
          val maybeProbability = ZIO.fromOption(EntitySearcher.getAnnotations(proposalAnnotationSubject, ontology, HasProbabilityAP).asScala.toSet[OWLAnnotation].collectFirst {
            case Annotation(_, HasProbabilityAP, value ^^ XSDDouble) =>
              ZIO.fromOption(value.toDoubleOption)
                .mapError(_ => BoomError(s"Proposal has probability value that can't be converted to a double: $proposalAnnotationSubject"))
          }).mapError(_ => BoomError(s"Can't find probability for proposal: $proposalAnnotationSubject")).flatten
          val proposalLabel = EntitySearcher.getAnnotations(proposalAnnotationSubject, ontology, RDFSLabel).asScala.toSet[OWLAnnotation].collectFirst {
            case Annotation(_, RDFSLabel, label ^^ _) => label
          }.getOrElse("")
          val proposalOWLAxioms = ontology.getAxioms(Imports.INCLUDED).asScala.toSet[OWLAxiom].collect {
            case axiom if axiom.getAnnotations(IsPartOfAP).asScala.exists(_.getValue == proposalAnnotationSubject) => axiom
          }
          val proposalWhelkAxioms = proposalOWLAxioms.flatMap(Bridge.convertAxiom).collect { case ci: ConceptInclusion => ci }
          val maybeGroup = ZIO.fromOption(EntitySearcher.getAnnotations(proposalAnnotationSubject, ontology, IsPartOfAP).asScala.toSet[OWLAnnotation].collectFirst {
            case Annotation(_, IsPartOfAP, anon: OWLAnonymousIndividual) => anon
            case Annotation(_, IsPartOfAP, iri: IRI)                     => Individual(iri)
          }).mapError(_ => BoomError(s"Can't find group for proposal: $proposalAnnotationSubject"))
          val maybeProposal = maybeProbability.map(p => Proposal(proposalLabel, proposalWhelkAxioms, p))
          for {
            (groups, axiomsToRemove) <- acc
            group <- maybeGroup
            proposal <- maybeProposal
          } yield {
            val currentProposals = groups.getOrElse(group, Set.empty)
            groups.updated(group, currentProposals + proposal) -> (axiomsToRemove ++ proposalOWLAxioms)
          }
      }
    proposals.map {
      case (proposalGroups, axiomsToRemove) =>
        val ontAxioms = (ontology.getAxioms(Imports.INCLUDED).asScala.toSet -- axiomsToRemove).flatMap(Bridge.convertAxiom)
        val groups = proposalGroups.values.map(ps => AlternativesGroup(ps)).toSet
        ProbabilisticOntology(ontAxioms, groups)
    }
  }

  def negateConceptInclusion(axiom: ConceptInclusion): Task[Set[ConceptInclusion]] = for {
    uuid <- ZIO.effect(UUID.randomUUID().toString)
    newClass = AtomicConcept(s"urn:uuid:$uuid")
  } yield Set(ConceptInclusion(newClass, axiom.subclass), ConceptInclusion(Conjunction(newClass, axiom.superclass), Bottom))

  /**
   * This produces the same axioms as negating the reversed concept inclusion (superclass/subclass).
   * It is specialized for two named classes so that it can produce a deterministic IRI for
   * the generated sibling class.
   */
  def disjointSibling(subclass: AtomicConcept, superclass: AtomicConcept): Set[ConceptInclusion] = {
    val text = s"${subclass.id}${superclass.id}"
    val hash = DigestUtils.sha1Hex(text)
    val sibling = AtomicConcept(s"$DisjointSiblingPrefix$hash")
    Set(ConceptInclusion(sibling, superclass), ConceptInclusion(Conjunction(sibling, subclass), Bottom))
  }

  //FIXME use prefix mappings, error handling
  private def parseCURIE(curie: String): String = {
    val items = curie.split(":", 2)
    val prefix = items(0).trim
    val id = items(1).trim
    s"http://purl.obolibrary.org/obo/${prefix}_$id"
  }

}
