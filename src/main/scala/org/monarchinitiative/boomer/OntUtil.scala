package org.monarchinitiative.boomer

import java.io.File
import java.util.UUID

import org.apache.commons.codec.digest.DigestUtils
import org.geneontology.whelk.BuiltIn.Bottom
import org.geneontology.whelk.{Individual => _, _}
import org.monarchinitiative.boomer.Boom.BoomError
import org.monarchinitiative.boomer.Model.{ProbabilisticOntology, Proposal, Uncertainty}
import org.openrdf.model.vocabulary.DCTERMS
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.search.EntitySearcher
import zio._
import zio.blocking._

import scala.io.Source
import scala.jdk.CollectionConverters._

object OntUtil {

  val BoomPrefix = "http://boom.monarchinitiative.org/vocab"
  val DisjointSiblingPrefix = s"$BoomPrefix/DisjointSibling"
  val HasProbability = "http://semanticscience.org/resource/SIO_000638"
  private val HasProbabilityAP = AnnotationProperty(HasProbability)
  private val IsPartOfAP = AnnotationProperty(DCTERMS.IS_PART_OF.stringValue)
  private val UncertaintyClass = Class(s"$BoomPrefix/Uncertainty")
  private val ProposalClass = Class(s"$BoomPrefix/Proposal")

  def readPTable(file: File, prefixes: Map[String, String]): ZIO[Blocking, Throwable, Set[Uncertainty]] =
    Task.effect(Source.fromFile(file, "utf-8")).bracket(Util.close(_)) { source =>
      ZIO.foreach(source.getLines().to(Iterable))(parsePTableLine(_, prefixes))
    }.map(_.toSet)

  private def parsePTableLine(line: String, prefixes: Map[String, String]): Task[Uncertainty] = {
    val columns = line.split("\\t", -1)
    if (columns.size == 6) {
      val leftCURIE = columns(0).trim
      val rightCURIE = columns(1).trim
      for {
        leftID <- ZIO.fromOption(expandCURIE(leftCURIE, prefixes)).mapError(_ => BoomError(s"Failed expanding CURIE: $leftCURIE"))
        rightID <- ZIO.fromOption(expandCURIE(rightCURIE, prefixes)).mapError(_ => BoomError(s"Failed expanding CURIE: $rightCURIE"))
        left = AtomicConcept(leftID)
        right = AtomicConcept(rightID)
        probProperSubLeftRight <- Task.effect(columns(2).trim.toDouble)
        probProperSubRightLeft <- Task.effect(columns(3).trim.toDouble)
        probEquivalent <- Task.effect(columns(4).trim.toDouble)
        probNoSubsumption <- Task.effect(columns(5).trim.toDouble)
        disjointSiblingOfLeftUnderRight = disjointSibling(left, right)
        disjointSiblingOfRightUnderLeft = disjointSibling(right, left)
      } yield {
        Uncertainty(Set(
          Proposal(s"$leftCURIE ProperSubClassOf $rightCURIE", disjointSiblingOfLeftUnderRight + ConceptInclusion(left, right), probProperSubLeftRight),
          Proposal(s"$leftCURIE ProperSuperClassOf $rightCURIE", disjointSiblingOfRightUnderLeft + ConceptInclusion(right, left), probProperSubRightLeft),
          Proposal(s"$leftCURIE EquivalentTo $rightCURIE", Set(ConceptInclusion(left, right), ConceptInclusion(right, left)), probEquivalent),
          Proposal(s"$leftCURIE SiblingOf $rightCURIE", disjointSiblingOfLeftUnderRight ++ disjointSiblingOfRightUnderLeft, probNoSubsumption)
        ).filter(_.probability > 0.0))
      }
    } else Task.fail(BoomError(s"Invalid ptable line: $line"))
  }

  def readProbabilisticOntology(file: File): ZIO[Blocking, Throwable, ProbabilisticOntology] = for {
    manager <- Task.effect(OWLManager.createOWLOntologyManager())
    inFile <- Task.effect(IRI.create(file))
    ontology <- effectBlocking(manager.loadOntology(inFile))
    po <- partitionOntology(ontology)
  } yield po

  def partitionOntology(ontology: OWLOntology): Task[ProbabilisticOntology] = {
    val groups = EntitySearcher.getInstances(UncertaintyClass, ontology).asScala.toSet[OWLIndividual].map(_ -> Set.empty[Proposal]).toMap
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
        val groups = proposalGroups.values.map(ps => Uncertainty(ps)).toSet
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
    val sibling = AtomicConcept(s"$DisjointSiblingPrefix#$hash")
    Set(ConceptInclusion(sibling, superclass), ConceptInclusion(Conjunction(sibling, subclass), Bottom))
  }

  /**
   * Convert the Whelk ConceptInclusion to an OWLSubClassOfAxiom, if both terms are named classes.
   *
   * @param ci axiom to convert
   * @return Some[OWLSubClassOfAxiom], or None if a term is an anonymous expression.
   */
  def whelkToOWL(ci: ConceptInclusion): Option[OWLSubClassOfAxiom] = ci match {
    case ConceptInclusion(AtomicConcept(sub), AtomicConcept(sup)) => Some(SubClassOf(Class(sub), Class(sup)))
    case _                                                        => None
  }

  private def expandCURIE(curie: String, prefixes: Map[String, String]): Option[String] = {
    val protocols = Set("http", "https", "ftp", "urn")
    val items = curie.split(":", 2)
    if (items.size > 1) {
      val prefix = items(0).trim
      val id = items(1).trim
      prefixes.get(prefix).map(ns => s"$ns$id").orElse {
        if (protocols(prefix)) Some(curie) else None
      }
    } else Some(curie)
  }

}
