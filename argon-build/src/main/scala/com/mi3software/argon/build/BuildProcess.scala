package com.mi3software.argon.build

import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import fs2._
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.impl.ParseHandler
import com.mi3software.argon.util.{CatsInstances, FileSpec}
import shims.effect._

object BuildProcess {

  trait ParsePhase[F[_]] {
    protected def findInputFiles: Stream[F, InputFileInfo[F]]

    def parseInput(implicit syncInstance: cats.effect.Sync[F]): EitherT[F, NonEmptyList[CompilationError], Vector[SourceAST]] =
      findInputFiles
        .translate(ParseHandler.addSyntaxErrorEffect[F, SyntaxErrorData])
        .flatMap { case InputFileInfo(fileSpec, dataStream) =>
          dataStream
            .translate(ParseHandler.addSyntaxErrorEffect[F, SyntaxError])
            .through(ParseHandler.parse[F](fileSpec))
            .translate(ParseHandler.convertSyntaxErrorToCompilationError(fileSpec))
        }
        .compile
        .toVector(CatsInstances.scalazEitherTSync)
        .leftMap { _.map[CompilationError](CompilationError.SyntaxCompilerError) }

  }
}
