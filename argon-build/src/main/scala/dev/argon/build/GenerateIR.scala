package dev.argon.build

import dev.argon.io.*
import dev.argon.compiler.*
import esexpr.{ESExpr, ESExprCodec}
import zio.*
import zio.stm.*

import java.io.IOException
import dev.argon.tube.resource.TubeResourceContext
import dev.argon.source.*
import dev.argon.vm.resource.VmIrResourceContext

abstract class GenerateIR extends CompileBase {
  override val context: CContext { type Error >: SourceError }

  val tubeResourceContext: TubeResourceContext & HasContext[context.type]
  import tubeResourceContext.TubeResource

  val vmirResourceContext: VmIrResourceContext & HasContext[context.type]
  import vmirResourceContext.VmIrResource


  def inputTube(using TubeImporter & HasContext[context.type]): TubeResource[context.Error]
  def referencedTubes(using TubeImporter & HasContext[context.type]): Seq[TubeResource[context.Error]]

  final case class IROutput(
    tube: VmIrResource[context.Error],
  )

  override type BuildOutput = IROutput

  final override protected def getReferencedTubes(using TubeImporter & HasContext[context.type]): ZIO[Scope & context.Env, context.Error, Seq[ArTube]] =
    ZIO.foreach(referencedTubes) { refTube =>
      refTube.asTube
    }

  final override protected def getCurrentTube(refTubes: Seq[ArTube])(using TubeImporter & HasContext[context.type]): ZIO[Scope & context.Env, context.Error, ArTube] =
    inputTube.asTube

  final override protected def createOutput(currentTube: ArTubeC & HasContext[context.type]): BuildOutput =
    IROutput(
      tube = new VmIrResource.Impl with Resource.WithoutFileName {
        protected override def tube: ArTube = currentTube
      },
    )


}
