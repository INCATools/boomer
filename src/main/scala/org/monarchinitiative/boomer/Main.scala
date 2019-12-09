package org.monarchinitiative.boomer

import java.io.{File, PrintWriter}

import org.geneontology.whelk.Bridge
import org.monarchinitiative.boomer.Model._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import zio._

object Main extends App {

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val program = for {
      start <- ZIO.effect(System.currentTimeMillis())
      ptable <- OntUtil.readPTable(new File(args(0)))
      ont <- ZIO.effect(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File(args(1)))))
      assertions = Bridge.ontologyToAxioms(ont)
      result <- Boom.evaluate(assertions, ptable)
      selections = result.flatMap(choices)
      writer <- ZIO.effect(new PrintWriter(new File("output.txt"), "utf-8"))
      _ <- ZIO.foreach(selections) { selection => ZIO.effect(writer.write(s"${selection.label}\n")) }
      _ <- ZIO.effect(writer.close())
      end <- ZIO.effect(System.currentTimeMillis())
      _ <- ZIO.effect(println(s"${(end - start) / 1000}s"))
    } yield ()
    program.fold(error => {
      error.printStackTrace()
      1
    }, _ => 0)
  }

  private def choices(selection: Selection): Set[Proposal] = selection match {
    case Init(_, _)                          => Set.empty
    case SelectedProposal(proposal, _, _, _)           => Set(proposal)
    case SelectedPerplexityProposal(selected, _, _, _) => selected.proposal.values.toSet
  }

}
