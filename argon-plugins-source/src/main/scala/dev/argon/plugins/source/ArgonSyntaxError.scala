package dev.argon.plugins.source

import dev.argon.parser.SyntaxError
import dev.argon.plugin.PluginException

class ArgonSyntaxError(syntaxError: SyntaxError) extends PluginException("source", "Syntax error: " + syntaxError.toString)
