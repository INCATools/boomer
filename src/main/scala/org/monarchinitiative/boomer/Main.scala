package org.monarchinitiative.boomer

import java.io.{File, FileOutputStream, FileReader, PrintWriter}
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import caseapp._
import io.circe.yaml.parser
import org.geneontology.whelk.{AtomicConcept, Bridge, Reasoner}
import org.monarchinitiative.boomer.Boom.{BoomError, BoomErrorMessage, ResolvedUncertainties}
import org.monarchinitiative.boomer.Model.Proposal
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
  outputInternalAxioms: Boolean = false,
  restrictOutputToPrefixes: List[String] = Nil
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
      whelkTask = ZIO.effectTotal(Reasoner.assert(assertions, Map.empty, false))
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
      exhaustiveResolvedCliques <- ZIO.foreachPar(doExhaustive) { runMappings =>
        Boom.evaluate(assertions, runMappings.map(_.uncertainty), prohibitedPrefixEquivalences, whelk, options.windowCount, 1, true)
      }
      shuffledResolvedCliques <- ZIO.foreachPar(doShuffled) { runMappings =>
        Boom.evaluate(assertions, runMappings.map(_.uncertainty), prohibitedPrefixEquivalences, whelk, options.windowCount, options.runs, false)
      }
      bestResolutionsForShuffledCliques = shuffledResolvedCliques.map(_.maxBy(Boom.jointProbability))
      exhaustiveResultsByClique = exhaustiveResolvedCliques.map(_.head)
      resultsByClique = Mapping.groupResultsByEquivalenceClique(exhaustiveResultsByClique ::: bestResolutionsForShuffledCliques,
                                                                equivCliques,
                                                                mappings)
      singletons = resultsByClique
        .filter { case (_, v) => v.size == 1 }
        .values
        .fold(Map.empty)(_ ++ _)
      consolidatedResultsByClique = resultsByClique.filterNot { case (_, v) => v.size == 1 } + (Set.empty[AtomicConcept] -> singletons)
      filteredResultsByClique = filterCliques(consolidatedResultsByClique, options, prefixes)
      _ <- ZIO.foreach_(filteredResultsByClique) { case (clique, resolved) => writeCliqueOutput(clique, resolved, options.output) }
      resolvedAsOne = (exhaustiveResolvedCliques.flatten ::: bestResolutionsForShuffledCliques).fold(ResolvedUncertainties.empty)((a, b) => a ++ b)
      _ <- ZIO.effect(scribe.info(s"Resolved size: ${resolvedAsOne.size}"))
      _ <- ZIO.effect(scribe.info(s"Most probable: ${resolvedAsOne.map(s => Math.log(s._2._1.probability)).sum}"))
      end <- clock.nanoTime
      _ <- ZIO.effect(scribe.info(s"${(end - start) / 1000000000}s"))
      selections = resolvedAsOne.values
      axioms = selections.flatMap(_._1.axioms.flatMap(axiom => OntUtil.whelkToOWL(axiom, !options.outputInternalAxioms))).to(Set)
      axiomsUsingEquivalence = OntUtil.collapseEquivalents(axioms)
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

  def filterCliques(cliques: Map[Set[AtomicConcept], ResolvedUncertainties],
                    options: Options,
                    prefixes: Map[String, String]): Map[Set[AtomicConcept], ResolvedUncertainties] =
    options.restrictOutputToPrefixes match {
      case prefix1 :: prefix2 :: Nil =>
        (for {
          namespace1 <- prefixes.get(prefix1)
          namespace2 <- prefixes.get(prefix2)
        } yield cliques
          .filter { case (terms, resolved) =>
            terms.exists(_.id.startsWith(namespace1)) &&
              terms.exists(_.id.startsWith(namespace2)) &&
              resolved.exists { case (uncertainty, (proposal, preferred)) =>
                proposal.axioms.exists(_.signature.exists(_.asInstanceOf[AtomicConcept].id.startsWith(namespace1))) &&
                  proposal.axioms.exists(_.signature.exists(_.asInstanceOf[AtomicConcept].id.startsWith(namespace2))) &&
                  !preferred
              }
          })
          .getOrElse(cliques)
      case Nil => cliques
      case _ =>
        scribe.warn("Ignoring more than two prefixes in output filter")
        cliques
    }

  def writeCliqueOutput(clique: Set[AtomicConcept], selections: ResolvedUncertainties, dirName: String): RIO[Blocking, Unit] = {
    val cliqueName = if (clique.isEmpty) "SINGLETONS" else clique.to(List).map(_.id).sorted.mkString(" ")
    ZIO.effect(new File(dirName).mkdirs()) *>
      ZIO.effect(new PrintWriter(new File(s"$dirName/${cliqueID(clique)}.txt"), "utf-8")).bracketAuto { writer =>
        effectBlocking(writer.println(s"## $cliqueName")) *>
          ZIO.foreach_(selections) { case (unc, (selection, best)) =>
            val isBestText = if (best) "(most probable)" else ""
            effectBlocking(writer.write(s"${selection.label}\t$isBestText\t${selection.probability}\n"))
          }
      }
  }

  private def cliqueID(clique: Set[AtomicConcept]): String =
    if (clique.isEmpty) "singletons"
    else {
      val label = clique.to(List).map(_.id).sorted.mkString
      String.format("%064x", new BigInteger(1, messageDigest.digest(label.getBytes(StandardCharsets.UTF_8))))
    }

  private def messageDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

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

//  private def writeHotSpots(hotspots: Map[Option[Set[AtomicConcept]], Map[Model.Uncertainty, Map[(Model.Proposal, Boolean), Int]]],
//                            outputName: String): ZIO[Blocking, Throwable, Unit] =
//    ZIO.effect(new PrintWriter(new File(s"$outputName-hotspots.txt"), "utf-8")).bracketAuto { writer =>
//      ZIO.foreach_(hotspots) { case (grouping, uncertaintyAndProposals) =>
//        val groupLabel = grouping.getOrElse(Set.empty).map(_.id).toSeq.sorted.mkString(" ")
//        effectBlocking(writer.println(s"Group: $groupLabel")) *>
//          ZIO.foreach_(uncertaintyAndProposals) { case (uncertainty, proposals) =>
//            ZIO.foreach_(proposals) { case ((proposal, isBest), count) =>
//              effectBlocking(writer.println(s"${proposal.label}\t[$count]"))
//            } *> effectBlocking(writer.println())
//          } *> effectBlocking(writer.println())
//      }
//    }

  private def groupByClique(mappings: Set[Mapping], cliques: Map[AtomicConcept, Set[AtomicConcept]]): Map[Set[AtomicConcept], Set[Mapping]] =
    mappings.groupBy(mapping => cliques.getOrElse(mapping.left, Set(mapping.left)))

}
