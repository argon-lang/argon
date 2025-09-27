package dev.argon.backend.jsApi

import dev.argon.backend.metadata.{BackendLoaderOptions, BackendMetadata}
import dev.argon.backend.{Backend, BackendException, BackendFactory}
import zio.{Scope, ZIO}

import java.io.IOException
import java.nio.file.Path

private[backend] final class JSApiBackendFactory(
  backendDir: Path,
  override val metadata: BackendMetadata,
  loaderOptions: BackendLoaderOptions.JSLoaderOptions,
) extends BackendFactory {
  override def load[E >: BackendException | IOException]: ZIO[Scope, E, Backend[E]] =
    JSApiBackendLoader.loadJSApiBackend(metadata.backend.name)(
      backendDir.resolve(loaderOptions.`import-path`).toUri.toURL,
      loaderOptions.`export-name`,
    )
}
