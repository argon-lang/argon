package dev.argon.plugins.js

import java.nio.charset.CharacterCodingException

type JSPluginError = JSParseException | JSGenerateException | JSObjectDecodeException | CharacterCodingException

final class JSParseException(cause: Throwable) extends Exception(cause)
final class JSGenerateException(cause: Throwable) extends Exception(cause)
final class JSObjectDecodeException(message: String) extends Exception(message)


