package dev.argon.backend

import dev.argon.backend.scalaApi.ScopedResource
import dev.argon.util.async.ErrorWrapper
import zio.*

object ScopedResourceWrap {
  def wrap[E, A](io: ZIO[Scope, E, A]): IO[E, ScopedResource[A]] =
    Scope.make.flatMap { scope =>
      io.provideEnvironment(ZEnvironment(scope))
        .foldCauseZIO(
          failure = cause => scope.close(Exit.failCause(cause)) *> ZIO.failCause(cause),
          success = a => ZIO.succeed(new ScopedResource[A] {
            override def get(): UIO[A] = ZIO.succeed(a)

            override def close(): UIO[Unit] = scope.close(Exit.unit)
          })
        )
    }
    
  def unwrap[E, A](res: IO[E, ScopedResource[A]]): ZIO[Scope, E, A] =
    res.withFinalizer(_.close()).flatMap(_.get())
}
