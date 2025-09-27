package dev.argon.backend

import dev.argon.backend.metadata.{BackendLoaderOptions, BackendMetadata}
import dev.argon.backend.sjs
import dev.argon.io.PathLike
import zio.{Scope, ZIO}

import java.io.IOException
import java.nio.file.Path
import scala.scalajs.js

private[backend] final class JSApiBackendFactory(
  backendDir: String,
  override val metadata: BackendMetadata,
  loaderOptions: BackendLoaderOptions.JSLoaderOptions,
) extends BackendFactory {
  override def load[E >: BackendException | IOException]: ZIO[Scope, E, Backend[E]] =
    ZIO.fromPromiseJS(js.`import`[js.Object](PathLike.join(backendDir, loaderOptions.`import-path`)))
      .mapError { ex => BackendException(s"Error loading backend: ${metadata.backend.name}", ex) }
      .flatMap { module =>
        val factory = module.asInstanceOf[js.Dynamic].selectDynamic(loaderOptions.`export-name`).asInstanceOf[sjs.BackendFactory]
        JSApiBackendLoader.loadJSApiBackend(metadata.backend.name)(factory)
      }
}
