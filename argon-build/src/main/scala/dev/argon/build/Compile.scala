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

  final override protected def getReferencedTubes(using TubeImporter & HasContext[context.type]): ZIO[Scope & context.Env, context.Error, Seq[ArTube]] =
    ZIO.foreach(referencedTubes) { refTube =>
      refTube.asTube
    }

  final override protected def getCurrentTube(refTubes: Seq[ArTube])(using TubeImporter & HasContext[context.type]): ZIO[Scope & context.Env, context.Error, ArTube] =
    val tubeOptions = SourceCodeTubeOptions(
      name = tubeName,
      referencedTubes = refTubes.iterator.map(_.name).toSet,
      sources = Seq(inputDir),
    )
    SourceTube.make(context)(tubeOptions)
  end getCurrentTube

  final override protected def createOutput(currentTube: ArTubeC & HasContext[context.type]): ZIO[Scope & context.Env, context.Error, BuildOutput] =
    ZIO.succeed(
      CompileOutput(
        tube = new TubeResource.Impl with Resource.WithoutFileName {
          override def asTube: ZIO[Scope, context.Error, ArTube] = ZIO.succeed(currentTube)
        },
      )
    )


}
