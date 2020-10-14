package org.monarchinitiative.boomer

import java.io.File
import java.util.UUID

import org.apache.commons.codec.digest.DigestUtils
import org.eclipse.rdf4j.model.vocabulary.DCTERMS
import org.geneontology.whelk.BuiltIn.Bottom
import org.geneontology.whelk.{Individual => _, _}
import org.monarchinitiative.boomer.Boom.BoomErrorMessage
import org.monarchinitiative.boomer.Model.{ProbabilisticOntology, Proposal, Uncertainty}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.search.EntitySearcher
import zio._
import zio.blocking._

import scala.jdk.CollectionConverters._

object OntUtil {

  val BoomPrefix = "http://boom.monarchinitiative.org/vocab"
  val DisjointSiblingPrefix = s"$BoomPrefix/DisjointSibling"
  val HasProbability = "http://semanticscience.org/resource/SIO_000638"
  private val HasProbabilityAP = AnnotationProperty(HasProbability)
  private val IsPartOfAP = AnnotationProperty(DCTERMS.IS_PART_OF.stringValue)
  private val UncertaintyClass = Class(s"$BoomPrefix/Uncertainty")
  private val ProposalClass = Class(s"$BoomPrefix/Proposal")

  def readProbabilisticOntology(file: File): ZIO[Blocking, Throwable, ProbabilisticOntology] = for {
    manager <- Task.effect(OWLManager.createOWLOntologyManager())
    inFile <- Task.effect(IRI.create(file))
    ontology <- effectBlocking(manager.loadOntology(inFile))
    po <- partitionOntology(ontology)
  } yield po

  def partitionOntology(ontology: OWLOntology): Task[ProbabilisticOntology] = {
    val groups = EntitySearcher.getInstances(UncertaintyClass, ontology).asScala.to(Set).map(_ -> Set.empty[Proposal]).toMap
    val proposals: Task[(Map[OWLIndividual, Set[Proposal]], Set[OWLAxiom])] =
      EntitySearcher.getInstances(ProposalClass, ontology).asScala.to(Set).foldLeft(Task.effect(groups -> Set.empty[OWLAxiom])) { case (acc, ind) =>
        val proposalAnnotationSubject = ind match {
          case anon: OWLAnonymousIndividual => anon
          case named: OWLNamedIndividual    => named.getIRI
        }
        val maybeProbability = ZIO
          .fromOption(
            EntitySearcher.getAnnotations(proposalAnnotationSubject, ontology, HasProbabilityAP).asScala.to(Set).collectFirst {
              case Annotation(_, HasProbabilityAP, value ^^ XSDDouble) =>
                ZIO
                  .fromOption(value.toDoubleOption)
                  .orElseFail(BoomErrorMessage(s"Proposal has probability value that can't be converted to a double: $proposalAnnotationSubject"))
            }
          )
          .orElseFail(BoomErrorMessage(s"Can't find probability for proposal: $proposalAnnotationSubject"))
          .flatten
        val proposalLabel = EntitySearcher
          .getAnnotations(proposalAnnotationSubject, ontology, RDFSLabel)
          .asScala
          .to(Set)
          .collectFirst { case Annotation(_, RDFSLabel, label ^^ _) =>
            label
          }
          .getOrElse("")
        val proposalOWLAxioms = ontology.getAxioms(Imports.INCLUDED).asScala.to(Set).collect {
          case axiom if axiom.getAnnotations(IsPartOfAP).asScala.exists(_.getValue == proposalAnnotationSubject) => axiom
        }
        val proposalWhelkAxioms = proposalOWLAxioms.flatMap(Bridge.convertAxiom).collect { case ci: ConceptInclusion => ci }
        val maybeGroup = ZIO
          .fromOption(
            EntitySearcher.getAnnotations(proposalAnnotationSubject, ontology, IsPartOfAP).asScala.to(Set).collectFirst {
              case Annotation(_, IsPartOfAP, anon: OWLAnonymousIndividual) => anon
              case Annotation(_, IsPartOfAP, iri: IRI)                     => Individual(iri)
            }
          )
          .orElseFail(BoomErrorMessage(s"Can't find group for proposal: $proposalAnnotationSubject"))
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
    proposals.map { case (proposalGroups, axiomsToRemove) =>
      val ontAxioms = (ontology.getAxioms(Imports.INCLUDED).asScala.to(Set) -- axiomsToRemove).flatMap(Bridge.convertAxiom)
      val groups = proposalGroups.values.map(ps => Uncertainty(ps)).to(Set)
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

  def expandCURIE(curie: String, prefixes: Map[String, String]): Option[String] = {
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
