package dev.argon.build

import dev.argon.io.*
import dev.argon.compiler.*
import zio.*

import dev.argon.tube.resource.TubeResourceContext
import dev.argon.source.*
import dev.argon.tube.encoder.TubeEncoderBase
import dev.argon.vm.resource.VmIrResource

abstract class GenerateIR extends CompileBase {
  override val context: CContext & TubeEncoderBase.EncodeContext { type Error >: SourceError }

  val tubeResourceContext: TubeResourceContext & HasContext[context.type]
  import tubeResourceContext.TubeResource


  def inputTube(using TubeImporter & HasContext[context.type]): TubeResource[context.Error]
  def referencedTubes(using TubeImporter & HasContext[context.type]): Seq[TubeResource[context.Error]]
  def platformId: String

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

  final override protected def createOutput(currentTube: ArTubeC & HasContext[context.type]): ZIO[Scope & context.Env, context.Error, BuildOutput] =
    for
      env <- ZIO.environment[context.Env]
    yield IROutput(
      tube = new VmIrResource.Impl[context.Error] with Resource.WithoutFileName {
        protected override val context: GenerateIR.this.context.type = GenerateIR.this.context
        protected override def environment: ZEnvironment[context.Env] = env

        protected override def tube: ArTube = currentTube
        protected override def platformId: String = GenerateIR.this.platformId
      },
    )


}
