package dev.argon.compiler_tests

import java.io.IOException
import dev.argon.build.BuildError
import dev.argon.source.SourceError
import dev.argon.tube.loader.TubeFormatException
import fs2.data.xml.XmlException
import dev.argon.io.xml.XmlDocumentCountException
import dev.argon.backend.BackendException

type TestError = IOException | BuildError | SourceError | BackendException |
  TubeFormatException | XmlException | XmlDocumentCountException | TestException

