package org.monarchinitiative.boomer

import java.io.File
import java.util.UUID

import org.geneontology.whelk.BuiltIn.Bottom
import org.geneontology.whelk.{AtomicConcept, Axiom, Bridge, ConceptInclusion, Conjunction}
import org.monarchinitiative.boomer.Boom.{AlternativeHypotheticals, BoomError, HypotheticalAxioms}
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

  //  def readProbabilisticOntology(file: File): ZIO[Blocking, Throwable, (Set[Axiom], Set[HypotheticalAxiom])] = for {
  //    manager <- Task.effect(OWLManager.createOWLOntologyManager())
  //    inFile <- Task.effect(IRI.create(file))
  //    ontology <- effectBlocking(manager.loadOntology(inFile))
  //    (assertedOWLAxioms, hypotheticalOWLAxioms) = ontology.getAxioms(Imports.INCLUDED).asScala.toSet.partitionMap(splitHypothetical)
  //    assertedAxioms = assertedOWLAxioms.flatMap(Bridge.convertAxiom)
  //    hypotheticalAxioms <- ZIO.sequence(hypotheticalOWLAxioms.map { case (ax, prob) =>
  //      ZIO.fromOption(Bridge.convertAxiom(ax).headOption.collect { case ci: ConceptInclusion => ci }).map(HypotheticalAxiom(_, prob))
  //        .mapError(_ => BoomError(s"Subclass axiom doesn't map one-to-one to Whelk concept inclusion: $ax"))
  //    })
  //  } yield (assertedAxioms, hypotheticalAxioms.toSet)

  private[this] def splitHypothetical(axiom: OWLAxiom): Either[OWLAxiom, (OWLSubClassOfAxiom, Double)] = axiom match {
    case sco @ SubClassOf(annotations, _, _) =>
      val maybeProbability = annotations.collectFirst {
        case Annotation(_, HasProbability, value ^^ XSDDouble) => value.toDoubleOption.getOrElse(0.0)
      }
      maybeProbability.map(prob => Right(sco -> prob)).getOrElse(Left(sco))
    case other                               => Left(other)
  }

  def readPTable(file: File): ZIO[Blocking, Throwable, Set[AlternativeHypotheticals]] = for {
    source <- Task.effect(Source.fromFile(file, "utf-8"))
    lines <- Task.effect(source.getLines())
    parsed <- Task.effect(lines.map(parsePTableLine).toList)
    entries <- ZIO.collectAll(parsed)
  } yield entries.toSet

  private def parsePTableLine(line: String): Task[AlternativeHypotheticals] = {
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
        AlternativeHypotheticals(Set(
          HypotheticalAxioms(s"$leftCURIE ProperSubClassOf $rightCURIE", rightNotSubOfLeft + ConceptInclusion(left, right), probProperSubLeftRight),
          HypotheticalAxioms(s"$rightCURIE ProperSubClassOf $leftCURIE", leftNotSubOfRight + ConceptInclusion(right, left), probProperSubRightLeft),
          HypotheticalAxioms(s"$leftCURIE EquivalentTo $rightCURIE", Set(ConceptInclusion(left, right), ConceptInclusion(right, left)), probEquivalent),
          HypotheticalAxioms(s"$leftCURIE SiblingOf $rightCURIE", leftNotSubOfRight ++ rightNotSubOfLeft, probNoSubsumption)
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

  //  private def findLiteralValue(model: Model, subject: Resource, predicate: URI): Task[Literal] = {
  //    val literals = model.filter(subject, predicate, null, null).asScala.map(_.getObject).collect {
  //      case l: Literal => l
  //    }
  //    ZIO.fromOption(literals.headOption).mapError(_ => BoomError(s"Could not find triple with appropriate value for s=$subject and p=$predicate"))
  //  }
  //
  //  private def findDoubleValue(model: Model, subject: Resource, predicate: URI): Task[Double] = {
  //    val doubles = model.filter(subject, predicate, null, null).asScala.map(_.getObject).collect {
  //      case l: Literal => Task.effect(l.doubleValue)
  //    }
  //    Task.require(BoomError("Could not find triple with double value"))(ZIO.sequence(doubles).map(_.headOption))
  //  }
  //
  //
  //  private def modelToOntology(model: Model): Task[Set[Axiom]] = ZIO.effect {
  //    val source = new RioMemoryTripleSource(model)
  //    val manager = OWLManager.createOWLOntologyManager()
  //    manager.setOntologyLoaderConfiguration(new OWLOntologyLoaderConfiguration().setMissingImportHandlingStrategy(MissingImportHandlingStrategy.THROW_EXCEPTION))
  //    manager.setIRIMappers(Collections.emptySet())
  //    val parser = new RioParserImpl(new RioRDFXMLDocumentFormatFactory())
  //    val ontology = manager.createOntology()
  //    parser.parse(source, ontology, new OWLOntologyLoaderConfiguration())
  //    Bridge.ontologyToAxioms(ontology)
  //  }
  //
  //  private def openInputStream(file: File): Task[FileInputStream] = ZIO.effect(new FileInputStream(file))
  //
  //  private def closeInputStream(input: InputStream): UIO[Unit] = UIO(input.close())

}
