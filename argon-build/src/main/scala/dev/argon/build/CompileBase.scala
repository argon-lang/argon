package dev.argon.build

import dev.argon.io.*
import dev.argon.compiler.*
import esexpr.{ESExpr, ESExprCodec}
import zio.*
import zio.stm.*

import java.io.IOException
import dev.argon.tube.resource.TubeResourceContext

trait CompileBase extends UsingContext {
  override val context: CContext

  type BuildOutput

  protected def getCurrentTube(referencedTubes: Seq[ArTube])(using TubeImporter & HasContext[context.type]): ZIO[Scope & context.Env, context.Error, ArTube]
  protected def getReferencedTubes(using TubeImporter & HasContext[context.type]): ZIO[Scope & context.Env, context.Error, Seq[ArTube]]
  protected def createOutput(currentTube: ArTube): ZIO[Scope & context.Env, context.Error, BuildOutput]


  def compile(): ZIO[Scope & context.Env, context.Error, BuildOutput] =
    compileImpl()

  private def compileImpl(): ZIO[Scope & context.Env, context.Error, BuildOutput] =
    for
      tubeImporter <- TubeImporterImpl(context)
      given (TubeImporter & LoadTube & HasContext[context.type]) = tubeImporter

      refTubes <- getReferencedTubes
      currentTube <- getCurrentTube(refTubes)
      _ <- tubeImporter.loadTube(currentTube)
      _ <- ZIO.foreachDiscard(refTubes)(tubeImporter.loadTube)
      output <- createOutput(currentTube)
    yield output

}
