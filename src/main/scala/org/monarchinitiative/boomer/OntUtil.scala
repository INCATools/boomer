package org.monarchinitiative.boomer

import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.databind.ObjectMapper
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.codec.digest.DigestUtils
import org.eclipse.rdf4j.model.vocabulary.DCTERMS
import org.geneontology.obographs.owlapi.FromOwl
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

import java.io.{File, FileOutputStream, PrintWriter}
import java.util.UUID
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

  /** This produces the same axioms as negating the reversed concept inclusion (superclass/subclass).
    * It is specialized for two named classes so that it can produce a deterministic IRI for
    * the generated sibling class.
    */
  def disjointSibling(subclass: AtomicConcept, superclass: AtomicConcept): Set[ConceptInclusion] = {
    val text = s"${subclass.id}${superclass.id}"
    val hash = DigestUtils.sha1Hex(text)
    val sibling = AtomicConcept(s"$DisjointSiblingPrefix#$hash")
    Set(ConceptInclusion(sibling, superclass), ConceptInclusion(Conjunction(sibling, subclass), Bottom))
  }

  /** Convert the Whelk ConceptInclusion to an OWLSubClassOfAxiom, if both terms are named classes.
    *
    * @param ci axiom to convert
    * @return Some[OWLSubClassOfAxiom], or None if a term is an anonymous expression.
    */
  def whelkToOWL(ci: ConceptInclusion, excludeInternal: Boolean): Option[OWLSubClassOfAxiom] = ci match {
    case ConceptInclusion(AtomicConcept(sub), AtomicConcept(sup))
        if !excludeInternal || (!sub.startsWith(DisjointSiblingPrefix)) && (!sup.startsWith(DisjointSiblingPrefix)) =>
      Some(SubClassOf(Class(sub), Class(sup)))
    case _ => None
  }

  /** If both SubClassOf(A,B) and SubClassOf(B,A) are in the input, replace both with EquivalentClasses(A,B)
    *
    * @param axioms input set of subclass axioms
    * @return set of equivalent class axioms and remaining subclass axioms
    */
  def collapseEquivalents(axioms: Set[OWLSubClassOfAxiom]): Set[OWLClassAxiom] =
    axioms.foldLeft(Set.empty[OWLClassAxiom]) { case (accum, axiom) =>
      val counterpart = SubClassOf(axiom.getSuperClass, axiom.getSubClass)
      if (accum(counterpart)) (accum - counterpart) + EquivalentClasses(axiom.getSubClass, axiom.getSuperClass)
      else accum + axiom
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

  def compactIRI(iri: String, prefixes: Map[String, String]): String =
    prefixes
      .collectFirst {
        case (prefix, expansion) if iri.startsWith(expansion) =>
          iri.replace(expansion, s"$prefix:")
      }
      .getOrElse(iri)

  def writeAsOBOJSONold(ontology: OWLOntology, filename: String): ZIO[Blocking, Throwable, Unit] =
    for {
      gd <- ZIO.effect(new FromOwl().generateGraphDocument(ontology))
      mapper = new ObjectMapper()
      _ = mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL)
      writer = mapper.writerWithDefaultPrettyPrinter
      _ <- ZIO.effect(new FileOutputStream(new File(filename))).bracketAuto { ontStream =>
        effectBlocking(writer.writeValue(ontStream, gd))
      }
    } yield ()

  def writeAsOBOJSON(ontology: OWLOntology,
                     labelIndex: Map[String, String],
                     prefixes: Map[String, String],
                     filename: String): ZIO[Blocking, Throwable, Unit] = {
    val obograph = asOBOGraphs(ontology, labelIndex, prefixes)
    ZIO.effect(new PrintWriter(new File(filename), "utf-8")).bracketAuto { writer =>
      effectBlockingIO(writer.write(obograph.asJson.deepDropNullValues.toString))
    }
  }

  def asOBOGraphs(ontology: OWLOntology, labelIndex: Map[String, String], prefixes: Map[String, String]): OBOGraphs = {
    val axioms = ontology.getAxioms().asScala
    val (allNodes, allEdges) = axioms.foldLeft((Set.empty[Node], Set.empty[Edge])) { case ((nodes, edges), axiom) =>
      axiom match {
        case SubClassOf(anns, Class(sub), Class(sup)) =>
          val subNode = Node(ID(compactIRI(sub.toString, prefixes)), "CLASS", labelIndex.get(sub.toString))
          val superNode = Node(ID(compactIRI(sup.toString, prefixes)), "CLASS", labelIndex.get(sup.toString))
          val edgeType = if (anns.nonEmpty) "is_a" else "asserted_is_a"
          val meta = Meta(anns.map(asPropertyValue).to(List))
          val edge = Edge(ID(compactIRI(sub.toString, prefixes)), ID(edgeType), ID(compactIRI(sup.toString, prefixes)), meta)
          (nodes + subNode + superNode) -> (edges + edge)
        case EquivalentClasses(anns, classes) =>
          classes.to(List).collect { case c: OWLClass => c }.grouped(2).foldLeft((nodes, edges)) { case ((nodesWithEquiv, edgesWithEquiv), pair) =>
            if (pair.size == 2) {
              val sub = pair(0).getIRI.toString
              val sup = pair(1).getIRI.toString
              val subNode = Node(ID(compactIRI(sub, prefixes)), "CLASS", labelIndex.get(sub))
              val superNode = Node(ID(compactIRI(sup, prefixes)), "CLASS", labelIndex.get(sup))
              val edgeType = if (anns.nonEmpty) "owl:equivalentClass" else "asserted_equivalent_class"
              val meta = Meta(anns.map(asPropertyValue).to(List))
              val edge = Edge(ID(compactIRI(sub, prefixes)), ID(edgeType), ID(compactIRI(sup, prefixes)), meta)
              (nodesWithEquiv + subNode + superNode) -> (edgesWithEquiv + edge)
            } else (nodesWithEquiv, edgesWithEquiv)
          }
        case _ => (nodes, edges)
      }
    }
    val equivalenceSets = axioms.collect { case e: OWLEquivalentClassesAxiom => e }.foldLeft(Set.empty[Set[ID]]) { case (idSets, next) =>
      val classes = next.getNamedClasses.asScala.to(Set).map(_.getIRI.toString).map(iri => ID(compactIRI(iri, prefixes)))
      idSets.find(_.intersect(classes).nonEmpty) match {
        case Some(idSet) => (idSets - idSet) + (idSet ++ classes)
        case None        => idSets + classes
      }
    }
    OBOGraphs(
      OBOGraph(
        ID(Option(ontology.getOntologyID.getOntologyIRI.orNull()).map(_.toString).getOrElse("http://example.org/anonymous")),
        allNodes.to(List),
        allEdges.to(List),
        equivalenceSets.map(ids => NodeSet(ids.to(List))).to(List)
      ) :: Nil
    )
  }

  def asPropertyValue(annotation: OWLAnnotation): PropertyValue =
    PropertyValue(ID(annotation.getProperty.getIRI.toString), annotation.getValue)

}
