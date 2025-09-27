package dev.argon.backend

import dev.argon.backend.metadata.{BackendLoaderOptions, BackendMetadata, BackendSchema}
import dev.argon.io.PathLike
import zio.{Scope, ZIO}

import scala.scalajs.js
import java.io.IOException

object BackendLoader {
  def load(backendDir: String, schema: BackendSchema): Option[BackendFactory] =
    schema.loaders
      .collectFirst {
        case loaderOptions: BackendLoaderOptions.JSLoaderOptions =>
          new JSApiBackendFactory(backendDir, schema.toBackendMetadata, loaderOptions)
      }
}
