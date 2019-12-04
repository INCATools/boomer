package org.monarchinitiative.boomer

import java.io.File
import java.util.UUID

import org.geneontology.whelk.BuiltIn.Bottom
import org.geneontology.whelk.{AtomicConcept, Axiom, Bridge, ConceptInclusion, Conjunction}
import org.monarchinitiative.boomer.Boom.{AlternativesGroup, BoomError, Proposal}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import zio._
import zio.blocking._
import org.phenoscape.scowl._

import scala.io.Source
import scala.jdk.CollectionConverters._

object OntUtil {

  private val HasProbability = AnnotationProperty("http://semanticscience.org/resource/SIO_000638")

  private[this] def splitHypothetical(axiom: OWLAxiom): Either[OWLAxiom, (OWLSubClassOfAxiom, Double)] = axiom match {
    case sco @ SubClassOf(annotations, _, _) =>
      val maybeProbability = annotations.collectFirst {
        case Annotation(_, HasProbability, value ^^ XSDDouble) => value.toDoubleOption.getOrElse(0.0)
      }
      maybeProbability.map(prob => Right(sco -> prob)).getOrElse(Left(sco))
    case other                               => Left(other)
  }

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
        rightNotSubOfLeft <- negateConceptInclusion(ConceptInclusion(right, left))
        leftNotSubOfRight <- negateConceptInclusion(ConceptInclusion(left, right))
      } yield {
        AlternativesGroup(Set(
          Proposal(s"$leftCURIE ProperSubClassOf $rightCURIE", rightNotSubOfLeft + ConceptInclusion(left, right), probProperSubLeftRight),
          Proposal(s"$rightCURIE ProperSubClassOf $leftCURIE", leftNotSubOfRight + ConceptInclusion(right, left), probProperSubRightLeft),
          Proposal(s"$leftCURIE EquivalentTo $rightCURIE", Set(ConceptInclusion(left, right), ConceptInclusion(right, left)), probEquivalent),
          Proposal(s"$leftCURIE SiblingOf $rightCURIE", leftNotSubOfRight ++ rightNotSubOfLeft, probNoSubsumption)
        ).filter(_.probability > 0.0))
      }
    } else Task.fail(BoomError(s"Invalid ptable line: $line"))
  }

  def negateConceptInclusion(axiom: ConceptInclusion): Task[Set[ConceptInclusion]] = for {
    uuid <- ZIO.effect(UUID.randomUUID().toString)
    newClass = AtomicConcept(uuid)
  } yield Set(ConceptInclusion(newClass, axiom.subclass), ConceptInclusion(Conjunction(newClass, axiom.superclass), Bottom))

  //FIXME use prefix mappings, error handling
  private def parseCURIE(curie: String): String = {
    val items = curie.split(":", 2)
    val prefix = items(0).trim
    val id = items(1).trim
    s"http://purl.obolibrary.org/obo/${prefix}_$id"
  }

}
