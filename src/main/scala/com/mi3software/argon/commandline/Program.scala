package com.mi3software.argon.commandline

import com.mi3software.argon.backend.Backend
import com.mi3software.argon.pipeline.Pipeline
import scalaz.effect.{IO, SafeApp}
import scalaz._
import Scalaz._

object Program extends SafeApp {
  override def runl(args: List[String]): IO[Unit] = args match {
    case "compile" :: tail => runCompilation(tail)
    case cmd :: _ => IO.putStrLn(s"Unknown command '$cmd'.")
    case _ => IO.putStrLn("Please specify a command.")
  }

  private def runCompilation(args: List[String]): IO[Unit] =
    parseCompileArgs(args) match {
      case CompileArguments(None, _, _, _) =>
        IO.putStrLn("Backend not specified (-b).")

      case CompileArguments(_, None, _, _) =>
        IO.putStrLn("Output file not specified (-o).")

      case CompileArguments(_, _, Vector(), _) =>
        IO.putStrLn("No input files were specified.")

      case CompileArguments(Some(FindBackend(backend)), Some(outputFile), inputFiles, references) =>
        Pipeline.run(inputFiles)(references)(outputFile)(backend)

      case CompileArguments(Some(backendId), _, _, _) =>
        IO.putStrLn(s"Invalid backend '$backendId'.")
    }

  private object FindBackend {
    private val backends: Vector[Backend] = Vector()

    def unapply(backendId: String): Option[Backend] =
      backends.find(_.id === backendId)
  }


  private final case class CompileArguments
  (
    backend: Option[String],
    outputFile: Option[String],
    inputFiles: Vector[String],
    references: Vector[String],
  )

  private def parseCompileArgs(args: List[String], argState: CompileArguments = CompileArguments(None, None, Vector(), Vector())): CompileArguments =
    args match {
      case "-b" :: backend :: tail => parseCompileArgs(tail, argState.copy(backend = Some(backend)))
      case "-o" :: outputFile :: tail => parseCompileArgs(tail, argState.copy(outputFile = Some(outputFile)))
      case "-r" :: refFile :: tail => parseCompileArgs(tail, argState.copy(references = argState.references :+ refFile))
      case arg :: tail => parseCompileArgs(tail, argState.copy(inputFiles = argState.inputFiles :+ arg))
      case Nil => argState
    }

}
