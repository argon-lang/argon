package dev.argon.tube.loader

import dev.argon.io.BinaryResource
import dev.argon.compiler.*
import dev.argon.tube as t
import esexpr.{ESExprCodec, ESExprBinaryDecoder, ESExprFormatException}
import dev.argon.util.async.ErrorWrapper
import dev.argon.util.{*, given}
import dev.argon.util.ErrorTestUtils.splitCause
import zio.*
import zio.stream.*
import java.io.IOException

object TubeLoader {
  def load(
    context: Context { type Error >: TubeFormatException | IOException },
    res: BinaryResource[context.Error],
  )(using TubeImporter & HasContext[context.type]): ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]] =
    ZIO.suspendSucceed {
      val errorContext = ErrorWrapper.Context[context.Error]()
      import errorContext.given

      val tubeEntries: ZStream[context.Env, context.Error, t.TubeFileEntry] =
        ESExprBinaryDecoder.readAll(res.asBytes)
          .catchAllCause { cause =>
            splitCause[ESExprFormatException, context.Error](cause) match {
              case Left(ex) => ZStream.fail(TubeFormatException("Could not parse tube as ESExpr binary format", ex))
              case Right(cause) => ZStream.failCause(cause)
            }
          }
          .mapZIO { entryExpr =>
            ZIO.fromEither(
              summon[ESExprCodec[t.TubeFileEntry]]
                .decode(entryExpr)
                .left.map(TubeFormatException("Could not decode ESExpr as tube", _))
            )
          }

      TubeDeserialized(context, tubeEntries)
    }


}
