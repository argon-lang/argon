package dev.argon.tube.loader

import dev.argon.io.{BinaryResource, ESExprDecodedBinaryStreamResource}
import dev.argon.compiler.*
import dev.argon.tube as t
import dev.argon.tube.{ExternMap, SupportedPlatform}
import esexpr.{ESExpr, ESExprBinaryDecoder, ESExprCodec, ESExprException, ESExprFormatException}
import dev.argon.util.async.ErrorWrapper
import dev.argon.util.{*, given}
import dev.argon.util.ErrorTestUtils.splitCause
import zio.*
import zio.stream.*

import java.io.IOException

object TubeLoader {

  type TubeLoadContext = Context {
    type Error >: TubeFormatException | IOException
    val implementations: Context.ImplementationExterns {
      type TubeMetadata = (Seq[SupportedPlatform], Map[String, ESExpr])
      type ExternFunction = ExternMap
    }
  }

  def load(
    context: TubeLoadContext,
    res: BinaryResource[context.Error],
  )(using TubeImporter & HasContext[context.type]): ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]] =
    ZIO.suspendSucceed {
      val errorContext = ErrorWrapper.Context[context.Error]()
      import errorContext.given

      val tubeEntries: ZStream[context.Env, context.Error, t.TubeFileEntry] =
        res.decode[[E] =>> ESExprDecodedBinaryStreamResource[E, t.TubeFileEntry]]
          .decoded
          .catchAllCause { cause =>
            splitCause[ESExprException, context.Error](cause) match {
              case Left(ex) => ZStream.fail(TubeFormatException("Could not parse tube as ESExpr binary format", ex))
              case Right(cause) => ZStream.failCause(cause)
            }
          }

      TubeDeserialized(context, tubeEntries)
    }


}
