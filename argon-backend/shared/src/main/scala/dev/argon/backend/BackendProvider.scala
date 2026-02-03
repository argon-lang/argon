package dev.argon.backend

import zio.{IO, ZIO, ZLayer}

trait BackendProvider {
  def getBackendFactory(name: String): IO[BackendException, BackendFactory]
  def all: Seq[BackendFactory]
}

object BackendProvider {
  def liveFromFactories(factories: Seq[BackendFactory]): ZLayer[Any, Nothing, BackendProvider] =
    ZLayer.succeed(
      new BackendProvider {
        private val factoryMap: Map[String, BackendFactory] =
          factories
            .view
            .map { bf => bf.metadata.backend.name -> bf }
            .toMap
        
        override def getBackendFactory(name: String): IO[BackendException, BackendFactory] =
          ZIO.fromEither(factoryMap.get(name).toRight(BackendException(s"Unknown backend: $name")))

        override def all: Seq[BackendFactory] = factories
      }
    )
}

