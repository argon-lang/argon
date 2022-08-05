package dev.argon.plugins.js

import dev.argon.compiler.tube.TubeName

import java.nio.charset.CharacterCodingException
import java.io.{IOException, PrintWriter, StringWriter}

type JSPluginError = JSParseError | JSGenerateError | JSObjectDecodeError | ImportPathNotSpecifiedError | CharacterCodingException | IOException

final class JSParseError(cause: Throwable) extends Exception(cause)
final class JSGenerateError(cause: Throwable) extends Exception(cause)
final class JSObjectDecodeError(message: String) extends Exception(message)
final class ImportPathNotSpecifiedError(tubeName: TubeName)


