package dev.argon.vm.resource

import dev.argon.vm
import dev.argon.vm.encoder.TubeEncoder
import dev.argon.compiler.*
import zio.*
import zio.stream.*
import dev.argon.io.*
import esexpr.ESExpr
import java.io.IOException
import esexpr.ESExprException
import dev.argon.tube.loader.TubeFormatException
import dev.argon.tube.loader.TubeLoader

sealed abstract class VmIrResourceContext extends UsingContext {
  override val context: Context { type Error >: IOException | TubeFormatException }

  protected val environment: ZEnvironment[context.Env]
  
  trait VmIrResource[+E] extends ESExprDecodedBinaryStreamResource[E, vm.TubeFileEntry]

  object VmIrResource {
    trait Impl extends VmIrResource[context.Error] with ESExprDecodedBinaryStreamResource.Impl[context.Error, vm.TubeFileEntry] {
      protected def tube: ArTube

      override def decoded: Stream[context.Error, vm.TubeFileEntry] =
        TubeEncoder.encode(context)(tube).provideEnvironment(environment)
    }

    given (using TubeImporter & HasContext[context.type]): BinaryResourceDecoder[VmIrResource, context.Error] with
      override def decode(resource: BinaryResource[context.Error]): VmIrResource[context.Error] =
        new VmIrResource[context.Error] {
          override def decoded: Stream[context.Error, vm.TubeFileEntry] =
            summon[BinaryResourceDecoder[[E >: context.Error | ESExprException] =>> ESExprDecodedBinaryStreamResource[E, vm.TubeFileEntry], context.Error | ESExprException]]
              .decode(resource)
              .decoded
              .mapError {
                case ex: ESExprException => TubeFormatException("Could not decode VMIR entry", ex)
                case e: context.Error => e
              }

          override def expr: Stream[context.Error, ESExpr] =
            summon[BinaryResourceDecoder[ESExprBinaryStreamResource, context.Error | ESExprException]]
              .decode(resource)
              .expr
              .mapError {
                case ex: ESExprException => TubeFormatException("Could not parse VMIR as ESExpr binary format", ex)
                case e: context.Error => e
              }

          override def asBytes: Stream[context.Error, Byte] =
            resource.asBytes

          override def fileName: Option[String] =
            resource.fileName


        }
    end given
  }

}

object VmIrResourceContext {
  def make(ctx: Context { type Error >: IOException | TubeFormatException }): ctx.Comp[VmIrResourceContext & HasContext[ctx.type]] =
    for
      env <- ZIO.environment[ctx.Env]
    yield new VmIrResourceContext {
      override val context: ctx.type = ctx
      protected override val environment: ZEnvironment[context.Env] = env
    }
}
