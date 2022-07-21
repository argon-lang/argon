package dev.argon.plugins.js

import dev.argon.compiler.tube.TubeName

import java.nio.charset.CharacterCodingException

type JSPluginError = JSParseError | JSGenerateError | JSObjectDecodeError | ImportPathNotSpecifiedError | CharacterCodingException

final class JSParseError(cause: Throwable)
final class JSGenerateError(cause: Throwable)
final class JSObjectDecodeError(message: String)
final class ImportPathNotSpecifiedError(tubeName: TubeName)


