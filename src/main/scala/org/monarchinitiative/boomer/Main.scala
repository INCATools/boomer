package org.monarchinitiative.boomer

import java.io.{File, FileOutputStream, PrintWriter}

import org.geneontology.whelk.Bridge
import org.monarchinitiative.boomer.Boom.BoomError
import org.monarchinitiative.boomer.Model._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLAxiom}
import zio._
import zio.blocking._

import scala.jdk.CollectionConverters._

object Main extends App {

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val program = for {
      start <- ZIO.effect(System.currentTimeMillis())
      ptable <- OntUtil.readPTable(new File(args(0)))
      ont <- ZIO.effect(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File(args(1)))))
      shuffle <- ZIO.fromOption(args(2).toBooleanOption).mapError(_ => BoomError("Need true or false for 'shuffle'"))
      assertions = Bridge.ontologyToAxioms(ont)
      result <- Boom.evaluate(assertions, ptable, shuffle)
      selections = result.flatMap(choices)
      axioms = selections.flatMap(_._1.axioms.flatMap(OntUtil.whelkToOWL))
      writer <- ZIO.effect(new PrintWriter(new File("output.txt"), "utf-8"))
      _ <- ZIO.foreach(selections) { case (selection, best) =>
        effectBlocking(writer.write(s"${selection.label}\t$best\n"))
      }
      _ <- ZIO.effect(writer.close())
      outputOntology <- ZIO.effect(OWLManager.createOWLOntologyManager().createOntology(axioms.toSet[OWLAxiom].asJava))
      ontStream <- ZIO.effect(new FileOutputStream(new File("output.ofn")))
      _ <- effectBlocking(outputOntology.getOWLOntologyManager.saveOntology(outputOntology, new FunctionalSyntaxDocumentFormat(), ontStream))
      _ <- ZIO.effect(ontStream.close())
      end <- ZIO.effect(System.currentTimeMillis())
      _ <- ZIO.effect(println(s"${(end - start) / 1000}s"))
    } yield ()
    program.fold(error => {
      error.printStackTrace()
      1
    }, _ => 0)
  }

  private def choices(selection: Selection): Set[(Proposal, Boolean)] = selection match {
    case Init(_, _)                                    => Set.empty
    case SelectedProposal(proposal, uncertainty, _, _) => Set((proposal, proposal == uncertainty.mostProbable))
    case SelectedPerplexityProposal(selected, _, _, _) =>
      selected.proposal.toSet[(Uncertainty, Proposal)].map { case (uncertainty, proposal) => (proposal, proposal == uncertainty.mostProbable) }
  }

}
