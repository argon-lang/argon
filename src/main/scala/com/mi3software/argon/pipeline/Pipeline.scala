package com.mi3software.argon.pipeline

import com.mi3software.argon.backend.Backend
import com.mi3software.argon.parser.ParseHandler
import com.mi3software.argon.util.{FileID, FileOperations, FileSpec}
import scalaz.effect.IO
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.{CompilationError, CompilationMessage}

object Pipeline {

  def printMessages(msgs: NonEmptyList[CompilationMessage]): IO[Unit] =
    msgs
      .traverseU { msg =>
        IO.putStrLn(msg.toString)
      }
      .map { _ => () }

  def run(inputFiles: Vector[String])(references: Vector[String])(outputFile: String)(backend: Backend): IO[Unit] =
    inputFiles.zipWithIndex
      .traverseU {
        case (filename, i) =>
          EitherT(
            FileOperations.fileFromName(filename)
              .flatMap(FileOperations.readAllText)
              .map(ParseHandler.parse(FileSpec(FileID(i), filename)))
          )
      }
      .run
      .flatMap {
        case -\/(syntaxErrors) =>
          printMessages(syntaxErrors.map(CompilationError.SyntaxCompilerError))

        case \/-(sourceASTs) =>
          val context = backend.createContext(sourceASTs.flatten)
          backend.getCompilationResult(context) match {
            case -\/(messages) =>
              printMessages(messages)

            case \/-(result) =>
              FileOperations.fileFromName(outputFile).flatMap(result.writeToFile)
          }
      }



}
