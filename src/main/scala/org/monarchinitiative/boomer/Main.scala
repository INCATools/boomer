package org.monarchinitiative.boomer

import caseapp._
import io.circe.yaml.parser
import org.geneontology.whelk.{AtomicConcept, Bridge, Reasoner, ReasonerState}
import org.monarchinitiative.boomer.Boom.{BoomError, BoomErrorMessage, ResolvedUncertainties}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{AxiomType, IRI, OWLAxiom, OWLOntology}
import scribe.Level
import zio.ZIO.ZIOAutoCloseableOps
import zio._
import zio.blocking._
import zio.console._

import java.io.{File, FileOutputStream, FileReader, PrintWriter}
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.jdk.CollectionConverters._

@AppName("boomer")
@ProgName("boomer")
final case class Options(
  @HelpMessage("Name used for folder to ouput clique JSON files; also basename for ontology and Markdown output files.")
  @ValueDescription("output files/dir name")
  @ExtraName("o")
  output: String = "output",
  @HelpMessage("TSV file containing table of mappings with probabilities.")
  @ValueDescription("filename")
  @ExtraName("t")
  ptable: String,
  @HelpMessage("OWL file containing all asserted/background axioms.")
  @ValueDescription("filename")
  @ExtraName("a")
  ontology: String,
  @HelpMessage(
    "YAML dictionary of prefix-to-expansion mappings for all prefixes used in the ptable. These namespaces are also used to check for new within-namespace equivalences.")
  @ValueDescription("filename")
  @ExtraName("p")
  prefixes: String,
  @HelpMessage(
    "Number of groups to split a clique of mappings into when shuffling between greedy search runs. Windows maintain their order; mappings within a window are shuffled.")
  @ValueDescription("positive integer")
  @ExtraName("w")
  windowCount: Int,
  @HelpMessage("Number of separate shuffled runs to conduct for each greedy search.")
  @ValueDescription("positive integer")
  @ExtraName("r")
  runs: Int,
  @HelpMessage("Maximum size clique for exhaustive search algorithm. Larger cliques use greedy search.")
  @ValueDescription("positive integer")
  @ExtraName("e")
  exhaustiveSearchLimit: Int = 20,
  @HelpMessage(
    "Include axioms used to enforce proper subclass relationships (e.g. generated disjoint sibling classes) in OWL output (default false).")
  @ValueDescription("bool")
  @ExtraName("e")
  outputInternalAxioms: Boolean = false,
  @HelpMessage(
    "Generate output only for cliques where a mapping between these two namespaces was resolved as something other than its highest probability option.")
  @ValueDescription("prefix strings (max of 2)")
  @ExtraName("e")
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
      labelIndex = indexLabels(ont)
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
      _ <- writeFilteredResultsToMarkdown(filteredResultsByClique, labelIndex, prefixes, options.output)
      _ <- ZIO.effect(new File(options.output).mkdirs())
      _ <- ZIO.foreach_(filteredResultsByClique) { case (clique, resolved) =>
        val cliqueName = cliqueID(clique)
        OntUtil.writeAsOBOJSON(resolvedUncertaintiesAsOntology(resolved, whelk, labelIndex, cliqueName),
                               labelIndex,
                               prefixes,
                               s"${options.output}/$cliqueName.json")
      }
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
      .catchAllCause(cause => putStrLn(cause.untraced.prettyPrint).exitCode.as(ExitCode.failure))
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

  private def groupByClique(mappings: Set[Mapping], cliques: Map[AtomicConcept, Set[AtomicConcept]]): Map[Set[AtomicConcept], Set[Mapping]] =
    mappings.groupBy(mapping => cliques.getOrElse(mapping.left, Set(mapping.left)))

  /** Map of IRI strings to label strings
    */
  private def indexLabels(ontology: OWLOntology): Map[String, String] =
    (for {
      AnnotationAssertion(_, RDFSLabel, term: IRI, value ^^ _) <- ontology.getAxioms(AxiomType.ANNOTATION_ASSERTION).asScala
    } yield term.toString -> value).to(Map)

  def writeFilteredResultsToMarkdown(filteredResultsByClique: Map[Set[AtomicConcept], ResolvedUncertainties],
                                     labelIndex: Map[String, String],
                                     prefixes: Map[String, String],
                                     filename: String): ZIO[Blocking, Throwable, Unit] =
    ZIO.effect(new PrintWriter(new File(s"$filename.md"), "utf-8")).bracketAuto { writer =>
      ZIO.foreach_(filteredResultsByClique) { case (cliqueSet, selections) =>
        val cliqueName =
          if (cliqueSet.isEmpty) "SINGLETONS"
          else
            cliqueSet
              .to(List)
              .map(_.id)
              .map(id => id -> labelIndex.getOrElse(id, OntUtil.compactIRI(id, prefixes)))
              .map { case (id, label) =>
                s"[$label]($id)"
              }
              .sorted
              .mkString(" ")
        effectBlocking(writer.println(s"## $cliqueName\n")) *>
          ZIO.foreach_(selections) { case (unc, (selection, best)) =>
            val isBestText = if (best) "(most probable)" else ""
            selection.label match {
              case mapping: MappingRelation =>
                val leftTerm = s"[${labelIndex.getOrElse(mapping.left.id, OntUtil.compactIRI(mapping.left.id, prefixes))}](${mapping.left.id})"
                val rightTerm = s"[${labelIndex.getOrElse(mapping.right.id, OntUtil.compactIRI(mapping.right.id, prefixes))}](${mapping.right.id})"
                effectBlocking(writer.write(s"- $leftTerm ${mapping.label} $rightTerm\t$isBestText\t${selection.probability}\n"))
              case _ => ZIO.fail(BoomErrorMessage("Unexpectedly a mapping selection doesn't have a MappingRelation object"))
            }
          } *>
          effectBlocking(writer.println())
      }
    }

  private val VizWidth = AnnotationProperty("https://w3id.org/kgviz/penwidth")

  def resolvedUncertaintiesAsOntology(resolved: ResolvedUncertainties,
                                      asserted: ReasonerState,
                                      labelIndex: Map[String, String],
                                      cliqueName: String): OWLOntology = {
    val allProposalsAxioms = resolved.values.to(Set).flatMap { case (proposal, _) =>
      val subClassAxioms = proposal.axioms.flatMap(OntUtil.whelkToOWL(_, true))
      val axioms = OntUtil.collapseEquivalents(subClassAxioms)
      axioms.map(ax => ax.getAnnotatedAxiom(Set(Annotation(VizWidth, proposal.probability * 10)).asJava))
    }
    val concepts = allProposalsAxioms.flatMap(_.getClassesInSignature.asScala).map(c => AtomicConcept(c.getIRI.toString))
    val givenAxioms = for {
      concept1 <- concepts
      concept2 <- concepts
      if concept1 != concept2
      axiom <-
        if (asserted.closureSubsBySubclass.getOrElse(concept1, Set.empty)(concept2)) {
          if (asserted.closureSubsBySuperclass.getOrElse(concept1, Set.empty)(concept2))
            Some(EquivalentClasses(Class(concept1.id), Class(concept2.id)))
          else Some(SubClassOf(Class(concept1.id), Class(concept2.id)))
        } else None
    } yield axiom
    val allClassAxioms = allProposalsAxioms ++ givenAxioms
    val labelAxioms = for {
      cls <- allClassAxioms.flatMap(_.getClassesInSignature.asScala)
      label <- labelIndex.get(cls.getIRI.toString)
    } yield AnnotationAssertion(RDFSLabel, cls, label)
    val allAxioms = allClassAxioms ++ labelAxioms
    OWLManager.createOWLOntologyManager().createOntology(allAxioms.asJava, IRI.create(s"urn:clique:$cliqueName"))
  }

}
