package dev.argon.plugins.scheme

import dev.argon.io.TextResource
import zio.stream.*
import java.nio.charset.CharacterCodingException
import zio.*
import java.io.IOException

abstract class SchemeSourceCodeResource[E >: CharacterCodingException | IOException] extends TextResource.Impl[E] {

  def asExprs: IO[E, Seq[SchemeExpr]]

  final override def asText: ZStream[Any, E, String] =
    ZStream("#!/usr/bin/scheme --program\n#!chezscheme\n") ++
      ZStream.fromIterableZIO(asExprs).flatMap { expr =>
        ZStream.unwrap(
          for
            queue <- Queue.bounded[Exit[Option[Nothing], String]](15)
            _ <- expr.writeTo(s => queue.offer(Exit.Success(s)).unit)
              .onExit {
                case Exit.Success(_) => queue.offer(Exit.Failure(Cause.fail(None)))
                case Exit.Failure(cause) => queue.offer(Exit.Failure(cause.map(Some.apply)))
              }
              .fork
          yield ZStream.fromQueue(queue).flattenExitOption
        )
      }

}
