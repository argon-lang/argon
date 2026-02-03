package dev.argon.vm.resource

import dev.argon.vm
import dev.argon.vm.encoder.TubeEncoder
import dev.argon.tube.encoder.TubeEncoderBase
import dev.argon.compiler.*
import dev.argon.io.*
import dev.argon.tube.loader.TubeFormatException
import zio.*
import zio.stream.*
import scala.reflect.TypeTest
import esexpr.{ESExpr, ESExprException}
import java.io.IOException

trait VmIrResource[+E] extends ESExprDecodedBinaryStreamResource[E, vm.TubeFileEntry]

object VmIrResource {
  trait Impl[E >: Context.Error0 | TubeFormatException <: Matchable] extends VmIrResource[E] with ESExprDecodedBinaryStreamResource.Impl[E, vm.TubeFileEntry] {
    protected val context: TubeEncoderBase.EncodeContext { type Error = E }
    protected def environment: ZEnvironment[context.Env]
    protected def tube: ArTubeC & HasContext[context.type]
    protected def platformId: String

    override def decoded: Stream[E, vm.TubeFileEntry] =
      TubeEncoder(platformId).encode(context)(tube).provideEnvironment(environment)
  }

  given [E >: TubeFormatException | IOException <: Matchable] => TypeTest[Any, E] => BinaryResourceDecoder[VmIrResource, E]:
    override def decode(resource: BinaryResource[E]): VmIrResource[E] =
      resource match
        case resource: VmIrResource[E] => resource
        case _ =>
          new VmIrResource[E] {
            override def decoded: Stream[E, vm.TubeFileEntry] =
              resource.decode[[E] =>> ESExprDecodedBinaryStreamResource[E, vm.TubeFileEntry]]
                .decoded
                .mapError {
                  case ex: ESExprException => TubeFormatException("Could not decode VMIR entry", ex)
                  case e: E => e
                }

            override def expr: Stream[E, ESExpr] =
              resource.decode[ESExprBinaryStreamResource]
                .expr
                .mapError {
                  case ex: ESExprException => TubeFormatException("Could not parse VMIR as ESExpr binary format", ex)
                  case e: E => e
                }

            override def asBytes: Stream[E, Byte] =
              resource.asBytes

            override def fileName: Option[String] =
              resource.fileName


          }
  end given
}
