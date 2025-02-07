package dev.argon.source

import java.nio.charset.CharacterCodingException
import dev.argon.parser.SyntaxError

type SourceError = CharacterCodingException | SyntaxError
