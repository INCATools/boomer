package org.monarchinitiative.boomer

import java.io.{File, PrintWriter}

import org.geneontology.whelk.Bridge
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import zio._

object Main extends App {

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val program = for {
      ptable <- OntUtil.readPTable(new File(args(0)))
      ont <- Task.effect(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File(args(1)))))
      assertions = Bridge.ontologyToAxioms(ont)
      result <- Boom.evaluate(assertions, ptable)
      (accepted, reasonerState) = result
      output = accepted.map(_.label).mkString("\n")
      writer <- ZIO.effect(new PrintWriter(new File("output.txt"), "utf-8"))
      _ <- ZIO.effect(writer.write(output))
      _ <- ZIO.effect(writer.close())
      //_ <- ZIO.collectAll(accepted.map(hyp => putStrLn(s"${hyp.label}")))
    } yield ()
    program.fold(error => {
      error.printStackTrace()
      1
    }, _ => 0)
  }

}
