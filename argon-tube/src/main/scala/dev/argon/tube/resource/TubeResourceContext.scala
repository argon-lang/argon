package dev.argon.tube.resource

import dev.argon.tube as t
import dev.argon.tube.encoder.TubeEncoderImpl
import dev.argon.compiler.*
import zio.*
import zio.stream.*
import dev.argon.io.*
import esexpr.ESExpr
import java.io.IOException
import esexpr.ESExprException
import dev.argon.tube.loader.TubeFormatException
import dev.argon.tube.loader.TubeLoader
import zio.Cause.Die

sealed abstract class TubeResourceContext extends UsingContext {
  override val context: Context { type Error >: IOException | TubeFormatException }

  protected val environment: ZEnvironment[context.Env]
  
  trait TubeResource[+E] extends ESExprDecodedBinaryStreamResource[E, t.TubeFileEntry] {
    def asTube: ZIO[Scope, E, ArTube]
  }

  object TubeResource {
    trait Impl extends TubeResource[context.Error] with ESExprDecodedBinaryStreamResource.Impl[context.Error, t.TubeFileEntry] {
      override def decoded: Stream[context.Error, t.TubeFileEntry] =
        ZStream.unwrapScoped(
          asTube.map { tube =>
            TubeEncoderImpl.encode(context)(tube).provideEnvironment(environment)
          }
        )
    }

    given (using TubeImporter & HasContext[context.type]): BinaryResourceDecoder[TubeResource, context.Error] with
      override def decode(resource: BinaryResource[context.Error]): TubeResource[context.Error] =
        new TubeResource[context.Error] {


          override def asTube: ZIO[Scope, context.Error, ArTube] =
            TubeLoader.load(context, resource)
              .provideSomeEnvironment[Scope](_ ++ environment)

          override def decoded: Stream[context.Error, t.TubeFileEntry] =
            summon[BinaryResourceDecoder[[E >: context.Error | ESExprException] =>> ESExprDecodedBinaryStreamResource[E, t.TubeFileEntry], context.Error | ESExprException]]
              .decode(resource)
              .decoded
              .mapError {
                case ex: ESExprException => TubeFormatException("Could not decode tube entry", ex)
                case e: context.Error => e
              }

          override def expr: Stream[context.Error, ESExpr] =
            summon[BinaryResourceDecoder[ESExprBinaryStreamResource, context.Error | ESExprException]]
              .decode(resource)
              .expr
              .mapError {
                case ex: ESExprException => TubeFormatException("Could not parse tube as ESExpr binary format", ex)
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

object TubeResourceContext {
  def make(ctx: Context { type Error >: IOException | TubeFormatException }): ctx.Comp[TubeResourceContext & HasContext[ctx.type]] =
    for
      env <- ZIO.environment[ctx.Env]
    yield new TubeResourceContext {
      override val context: ctx.type = ctx
      protected override val environment: ZEnvironment[context.Env] = env
    }
}
