package dev.argon.backend.backends.js

import dev.argon.backend.*
import java.io.IOException
import zio.*

private[js] trait JSBackendFactoryPlatformSpecific {
  def load[E >: BackendException | IOException]: ZIO[Scope, E, Backend[E]] =
    JSApiBackendLoader.loadJSApiBackend("js")(
      JSBackendModule.backendFactory
    )

}
