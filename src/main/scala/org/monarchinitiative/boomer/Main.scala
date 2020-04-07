package org.monarchinitiative.boomer

import java.io.{File, FileOutputStream, FileReader, PrintWriter}

import caseapp._
import io.circe.yaml.parser
import org.geneontology.whelk.Bridge
import org.monarchinitiative.boomer.Boom.BoomError
import org.monarchinitiative.boomer.Util.close
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLAxiom}
import zio._
import zio.blocking._
import zio.console._

import scala.jdk.CollectionConverters._

object Main extends App {

  case class Options(
                      ptable: String,
                      ontology: String,
                      prefixes: String,
                      windowCount: Int,
                      runs: Int
                    )

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val program = for {
      start <- ZIO.effect(System.currentTimeMillis())
      parsed <- ZIO.fromEither(CaseApp.parse[Options](args))
      (options, remainder) = parsed
      prefixes <- prefixesFromFile(options.prefixes).filterOrFail(checkNamespacesNonOverlapping)(BoomError("No namespace should be contained within another; this will interfere with equivalence constraints."))
      ptable <- OntUtil.readPTable(new File(options.ptable), prefixes)
      ont <- ZIO.effect(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File(options.ontology))))
      assertions = Bridge.ontologyToAxioms(ont)
      results <- Boom.evaluate(assertions, ptable, prefixes.values.toSet, options.windowCount, options.runs)
      (mostProbable, counted) = Boom.organizeResults(results)
      _ <- ZIO.effect(scribe.info(s"Most probable: ${mostProbable.map(s => Math.log(s._2._1.probability)).sum}"))
      hotspots = counted.filter { case (_, proposalsToCounts) => proposalsToCounts.size > 1 }
      _ <- writeHotSpots(hotspots)
      selections = mostProbable.values
      axioms = selections.flatMap(_._1.axioms.flatMap(OntUtil.whelkToOWL))
      _ <- ZIO.effect(new PrintWriter(new File("output.txt"), "utf-8")).bracket(close(_)) { writer =>
        ZIO.foreach(selections) { case (selection, best) =>
          effectBlocking(writer.write(s"${selection.label}\t$best\n"))
        }
      }
      outputOntology <- ZIO.effect(OWLManager.createOWLOntologyManager().createOntology(axioms.toSet[OWLAxiom].asJava))
      _ <- ZIO.effect(new FileOutputStream(new File("output.ofn"))).bracket(close(_)) { ontStream =>
        effectBlocking(outputOntology.getOWLOntologyManager.saveOntology(outputOntology, new FunctionalSyntaxDocumentFormat(), ontStream))
      }
      end <- ZIO.effect(System.currentTimeMillis())
      _ <- ZIO.effect(scribe.info(s"${(end - start) / 1000}s"))
    } yield ()
    program.as(0).catchSome {
      case e: caseapp.core.Error => ZIO.effect(scribe.error(e.message))
      case BoomError(msg)        => ZIO.effect(scribe.error(msg))
    }.as(1).catchAllCause(cause => putStrLn(cause.untraced.prettyPrint).as(1))
  }

  private def prefixesFromFile(filename: String): Task[Map[String, String]] =
    ZIO.effect(new FileReader(new File(filename))).bracket(close(_)) { reader =>
      ZIO.fromEither(parser.parse(reader)).flatMap { json =>
        ZIO.fromEither(json.as[Map[String, String]])
      }
    }

  private def checkNamespacesNonOverlapping(prefixes: Map[String, String]): Boolean = {
    (for {
      ns1 <- prefixes.values
      ns2 <- prefixes.values
      if ns1 != ns2
      if ns1.contains(ns2)
    } yield (ns1, ns2)).isEmpty
  }

  private def writeHotSpots(hotspots: Map[Model.Uncertainty, Map[(Model.Proposal, Boolean), Int]]): ZIO[Blocking, Throwable, Unit] = {
    ZIO.effect(new PrintWriter(new File("output-hotspots.txt"), "utf-8")).bracket(close(_)) { writer =>
      ZIO.foreach_(hotspots) { case (uncertainty, proposals) =>
        ZIO.foreach_(proposals) { case ((proposal, isBest), count) =>
          val isBestText = if (isBest) " (most probable)" else ""
          effectBlocking(writer.println(s"${proposal.label}$isBestText\t[$count]"))
        } *> effectBlocking(writer.println())
      }
    }
  }

}
