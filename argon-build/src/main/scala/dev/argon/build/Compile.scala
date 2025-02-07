package dev.argon.build

import dev.argon.io.*
import dev.argon.compiler.*
import esexpr.{ESExpr, ESExprCodec}
import zio.*
import zio.stm.*

import java.io.IOException
import dev.argon.tube.resource.TubeResourceContext
import dev.argon.source.*

abstract class Compile extends CompileBase {
  override val context: CContext { type Error >: SourceError }

  val tubeResourceContext: TubeResourceContext & HasContext[context.type]
  import tubeResourceContext.TubeResource


  def tubeName: TubeName
  def inputDir: DirectoryResource[context.Error, ArgonSourceCodeResource]
  def referencedTubes(using TubeImporter & HasContext[context.type]): Seq[TubeResource[context.Error]]

  final case class CompileOutput(
    tube: TubeResource[context.Error],
  )

  override type BuildOutput = CompileOutput



  protected override def compileImpl(): ZIO[context.Env & Scope, context.Error, BuildOutput] =
    for
      tubeImporter <- TubeImporterImpl(context)
      given (TubeImporter & LoadTube & HasContext[context.type]) = tubeImporter


      refTubes <- ZIO.foreach(referencedTubes) { refTube =>
        refTube.asTube
      }

      tubeOptions = SourceCodeTubeOptions(
        name = tubeName,
        referencedTubes = refTubes.iterator.map(_.name).toSet,
        sources = Seq(inputDir),
      )

      sourceTube <- SourceTube.make(context)(tubeOptions)

      _ <- tubeImporter.loadTube(sourceTube)
      _ <- ZIO.foreachDiscard(refTubes)(tubeImporter.loadTube)
            
    yield CompileOutput(
      tube = new TubeResource.Impl with Resource.WithoutFileName {
        override def asTube: ZIO[Scope, context.Error, ArTube] = ZIO.succeed(sourceTube)
      },
    )



}
