package org.monarchinitiative.boomer

import java.io.File
import java.util.UUID

import org.apache.commons.codec.digest.DigestUtils
import org.geneontology.whelk.BuiltIn.Bottom
import org.geneontology.whelk.{AtomicConcept, ConceptInclusion, Conjunction}
import org.monarchinitiative.boomer.Boom.BoomError
import org.monarchinitiative.boomer.Model.{Uncertainty, Proposal}
import zio._
import zio.blocking._

import scala.io.Source

object OntUtil {

  val DisjointSiblingPrefix = "http://boom.monarchinitiative.org/vocab/disjoint_sibling#"

  def readPTable(file: File): ZIO[Blocking, Throwable, Set[Uncertainty]] = for {
    source <- Task.effect(Source.fromFile(file, "utf-8"))
    lines <- Task.effect(source.getLines())
    parsed <- Task.effect(lines.map(parsePTableLine).toList)
    entries <- ZIO.collectAll(parsed)
  } yield entries.toSet

  private def parsePTableLine(line: String): Task[Uncertainty] = {
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
        Uncertainty(Set(
          Proposal(s"$leftCURIE ProperSubClassOf $rightCURIE", disjointSiblingOfLeftUnderRight + ConceptInclusion(left, right), probProperSubLeftRight),
          Proposal(s"$leftCURIE ProperSuperClassOf $rightCURIE", disjointSiblingOfRightUnderLeft + ConceptInclusion(right, left), probProperSubRightLeft),
          Proposal(s"$leftCURIE EquivalentTo $rightCURIE", Set(ConceptInclusion(left, right), ConceptInclusion(right, left)), probEquivalent),
          Proposal(s"$leftCURIE SiblingOf $rightCURIE", disjointSiblingOfLeftUnderRight ++ disjointSiblingOfRightUnderLeft, probNoSubsumption)
        ).filter(_.probability > 0.0))
      }
    } else Task.fail(BoomError(s"Invalid ptable line: $line"))
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
