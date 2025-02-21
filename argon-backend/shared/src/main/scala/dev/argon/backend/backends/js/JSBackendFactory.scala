package dev.argon.backend.backends.js

import dev.argon.backend.*
import dev.argon.backend.metadata.*

object JSBackendFactory extends BackendFactory with JSBackendFactoryPlatformSpecific {
  override val metadata: BackendMetadata =
    BackendMetadata(
      backend = BackendBackendMetadata(
        `api-version` = "0.1.0",
        name = "js",
      ),

      options = BackendOptionsSchema(
        codegen = Map(
          "externs" -> BackendOption(
            `type` = OptionType.BinaryResource,
            occurrence = OptionOccurrence.Many,
          ),
        ),
        output = Map(
          "modules" -> BackendOptionOutput(
            `type` = OutputType.DirectoryResource,
          ),
        ),
      ),
    )
}
