package org.monarchinitiative.boomer

import caseapp.Name
import caseapp.core.help.{Help, WithHelp}
import caseapp.core.parser.Parser
import caseapp.core.util.Formatter
import caseapp.core.{Error, RemainingArgs}
import zio._
import zio.console.{putStrLn, Console}

/** Adapted from caseapp.cats.IOCaseApp
  */
abstract class ZCaseApp[T](implicit val parser0: Parser[T], val messages: Help[T]) extends App {

  private[this] def parser: Parser[T] = {
    val p = parser0.nameFormatter(nameFormatter)
    if (stopAtFirstUnrecognized)
      p.stopAtFirstUnrecognized
    else
      p
  }

  def run(options: T, remainingArgs: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode]

  private[this] def error(message: Error): ZIO[Console, Nothing, ExitCode] =
    putStrLn(message.message).as(ExitCode.failure)

  private[this] def helpAsked: ZIO[Console, Nothing, ExitCode] =
    putStrLn(messages.withHelp.help).as(ExitCode.success)

  private[this] def usageAsked: ZIO[Console, Nothing, ExitCode] =
    putStrLn(messages.withHelp.usage).as(ExitCode.success)

  /** Arguments are expanded then parsed. By default, argument expansion is the identity function.
    * Overriding this method allows plugging in an arbitrary argument expansion logic.
    *
    * One such expansion logic involves replacing each argument of the form '@<file>' with the
    * contents of that file where each line in the file becomes a distinct argument.
    * To enable this behavior, override this method as shown below.
    *
    * @example
    * {{{
    * import caseapp.core.parser.PlatformArgsExpander
    * override def expandArgs(args: List[String]): List[String]
    * = PlatformArgsExpander.expand(args)
    * }}}
    *
    * @param args
    * @return
    */
  private[this] def expandArgs(args: List[String]): List[String] = args

  /** Whether to stop parsing at the first unrecognized argument.
    *
    * That is, stop parsing at the first non option (not starting with "-"), or
    * the first unrecognized option. The unparsed arguments are put in the `args`
    * argument of `run`.
    */
  private[this] def stopAtFirstUnrecognized: Boolean = false

  private[this] def nameFormatter: Formatter[Name] = Formatter.DefaultNameFormatter

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    parser.withHelp.detailedParse(expandArgs(args), stopAtFirstUnrecognized) match {
      case Left(err)                                        => error(err)
      case Right((WithHelp(_, true, _), _))                 => helpAsked
      case Right((WithHelp(true, _, _), _))                 => usageAsked
      case Right((WithHelp(_, _, Left(err)), _))            => error(err)
      case Right((WithHelp(_, _, Right(t)), remainingArgs)) => run(t, remainingArgs)
    }

}
