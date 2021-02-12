package org.monarchinitiative.boomer

import java.io.{File, FileOutputStream, FileReader, PrintWriter}

import caseapp._
import io.circe.yaml.parser
import org.geneontology.whelk.{AtomicConcept, Bridge, Reasoner}
import org.monarchinitiative.boomer.Boom.{BoomError, BoomErrorMessage, ResolvedUncertainties}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLAxiom}
import scribe.Level
import zio.ZIO.ZIOAutoCloseableOps
import zio._
import zio.blocking._
import zio.console._

import scala.jdk.CollectionConverters._

final case class Options(
  @ExtraName("o")
  output: String = "output",
  @ExtraName("t")
  ptable: String,
  @ExtraName("a")
  ontology: String,
  @ExtraName("p")
  prefixes: String,
  @ExtraName("w")
  windowCount: Int,
  @ExtraName("r")
  runs: Int,
  exhaustiveSearchLimit: Int = 20,
  outputInternalAxioms: Boolean = false
)

object Main extends ZCaseApp[Options] {

  override def run(options: Options, arg: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode] = {
    val program = for {
      _ <- ZIO.effectTotal(scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Info)).replace())
      start <- clock.nanoTime
      prefixes <- prefixesFromFile(options.prefixes).filterOrFail(checkNamespacesNonOverlapping)(
        BoomErrorMessage("No namespace should be a lexical substring of another; this will interfere with equivalence constraints."))
      mappings <- Mapping.readPTable(new File(options.ptable), prefixes)
      ont <- ZIO.effect(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File(options.ontology))))
      assertions = Bridge.ontologyToAxioms(ont)
      prohibitedPrefixEquivalences = prefixes.values.to(Set)
      whelkTask = ZIO.effectTotal(
        Reasoner.assert(assertions, Map(NamespaceChecker.DelegateKey -> NamespaceChecker(prohibitedPrefixEquivalences, Nil))))
      equivCliquesTask = ZIO.effectTotal(Mapping.makeMaximalEquivalenceCliques(mappings, assertions))
      (equivCliques, whelk) <- ZIO.tupledPar(equivCliquesTask, whelkTask)
      _ <- putStrLn(s"Num equiv cliques: ${equivCliques.values.toSet.size}")
      grouped = groupByClique(mappings, equivCliques)
      _ <- ZIO.effect(scribe.info(s"Num mapping cliques: ${grouped.size}"))
      _ <- ZIO.foreach_(grouped) { case (grouping, mappingGroup) =>
        putStrLn(mappingGroup.size.toString)
      }
      runs = grouped.values.to(List)
      (doExhaustive, doShuffled) = runs.partition(_.size <= options.exhaustiveSearchLimit)
      exhaustiveResolvedCliques <- ZIO.foreach(doExhaustive) { runMappings => //FIXME par
        Boom.evaluate(assertions, runMappings.map(_.uncertainty), prefixes.values.to(Set), whelk, options.windowCount, 1, true)
      }
      shuffledResolvedCliques <- ZIO.foreachPar(doShuffled) { runMappings =>
        Boom.evaluate(assertions, runMappings.map(_.uncertainty), prefixes.values.to(Set), whelk, options.windowCount, options.runs, false)
      }
      bestResolutionsForShuffledCliques = shuffledResolvedCliques.map(_.maxBy(Boom.jointProbability))
      resolvedAsOne = (exhaustiveResolvedCliques.flatten ::: bestResolutionsForShuffledCliques).fold(ResolvedUncertainties.empty)((a, b) => a ++ b)
      _ <- ZIO.effect(scribe.info(s"Resolved size: ${resolvedAsOne.size}"))
      _ <- ZIO.effect(scribe.info(s"Most probable: ${resolvedAsOne.map(s => Math.log(s._2._1.probability)).sum}"))
      end <- clock.nanoTime
      _ <- ZIO.effect(scribe.info(s"${(end - start) / 1000000000}s"))
      selections = resolvedAsOne.values
      axioms = selections.flatMap(_._1.axioms.flatMap(axiom => OntUtil.whelkToOWL(axiom, !options.outputInternalAxioms))).to(Set)
      axiomsUsingEquivalence = OntUtil.collapseEquivalents(axioms)
      _ <- ZIO.effect(new PrintWriter(new File(s"${options.output}.txt"), "utf-8")).bracketAuto { writer =>
        ZIO.foreach(selections) { case (selection, best) =>
          val isBestText = if (best) "(most probable)" else ""
          effectBlocking(writer.write(s"${selection.label}\t$isBestText\t${selection.probability}\n"))
        }
      }
      outputOntology <- ZIO.effect(OWLManager.createOWLOntologyManager().createOntology(axiomsUsingEquivalence.toSet[OWLAxiom].asJava))
      _ <- ZIO.effect(new FileOutputStream(new File(s"${options.output}.ofn"))).bracketAuto { ontStream =>
        effectBlocking(outputOntology.getOWLOntologyManager.saveOntology(outputOntology, new FunctionalSyntaxDocumentFormat(), ontStream))
      }
    } yield ()
    program
      .as(ExitCode.success)
      .catchSome { case BoomError(msg) =>
        ZIO.effectTotal(scribe.error(msg)).as(ExitCode.failure)
      }
      .catchAllCause(cause => putStrLn(cause.untraced.prettyPrint).as(ExitCode.failure))

  }

  def prefixesFromFile(filename: String): Task[Map[String, String]] =
    ZIO.effect(new FileReader(new File(filename))).bracketAuto { reader =>
      ZIO.fromEither(parser.parse(reader)).flatMap { json =>
        ZIO.fromEither(json.as[Map[String, String]])
      }
    }

  def checkNamespacesNonOverlapping(prefixes: Map[String, String]): Boolean =
    (for {
      ns1 <- prefixes.values
      ns2 <- prefixes.values
      if ns1 != ns2
      if ns1.contains(ns2)
    } yield (ns1, ns2)).isEmpty

  private def writeHotSpots(hotspots: Map[Option[Set[AtomicConcept]], Map[Model.Uncertainty, Map[(Model.Proposal, Boolean), Int]]],
                            outputName: String): ZIO[Blocking, Throwable, Unit] =
    ZIO.effect(new PrintWriter(new File(s"$outputName-hotspots.txt"), "utf-8")).bracketAuto { writer =>
      ZIO.foreach_(hotspots) { case (grouping, uncertaintyAndProposals) =>
        val groupLabel = grouping.getOrElse(Set.empty).map(_.id).toSeq.sorted.mkString(" ")
        effectBlocking(writer.println(s"Group: $groupLabel")) *>
          ZIO.foreach_(uncertaintyAndProposals) { case (uncertainty, proposals) =>
            ZIO.foreach_(proposals) { case ((proposal, isBest), count) =>
              effectBlocking(writer.println(s"${proposal.label}\t[$count]"))
            } *> effectBlocking(writer.println())
          } *> effectBlocking(writer.println())
      }
    }

  private def groupByClique(mappings: Set[Mapping], cliques: Map[AtomicConcept, Set[AtomicConcept]]): Map[Set[AtomicConcept], Set[Mapping]] =
    mappings.groupBy(mapping => cliques.getOrElse(mapping.left, Set(mapping.left)))

}
