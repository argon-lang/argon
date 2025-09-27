package dev.argon.backend

import dev.argon.backend.metadata.{BackendLoaderOptions, BackendSchema}

import java.nio.file.Path

object BackendLoader {
  def load(backendDir: Path, schema: BackendSchema): Option[BackendFactory] =
    schema.loaders
      .collectFirst {
        case loaderOptions: BackendLoaderOptions.JSLoaderOptions =>
          JSApiBackendFactory(backendDir, schema.toBackendMetadata, loaderOptions)
      }
}
