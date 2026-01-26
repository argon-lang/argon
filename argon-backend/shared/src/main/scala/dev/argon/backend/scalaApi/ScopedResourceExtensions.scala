package dev.argon.backend.scalaApi

import zio.*

object ScopedResourceExtensions {

  extension (_c: ScopedResource.type)
    def fromScopeIO[E, A](io: ZIO[Scope, E, A]): UIO[ScopedResource[E, A]] =
      for
        scope <- Scope.make
        io <- io.memoize
      yield new ScopedResource[E, A] {
        override def get(): IO[E, A] =
          io.provideEnvironment(ZEnvironment(scope))

        override def close(): UIO[Unit] =
          scope.close(Exit.unit)
      }
  end extension


  extension [E, A](sr: ScopedResource[E, A])
    def toScopeIO: ZIO[Scope, E, A] =
      ZIO.acquireRelease(sr.get())(_ => sr.close())
  end extension

}
