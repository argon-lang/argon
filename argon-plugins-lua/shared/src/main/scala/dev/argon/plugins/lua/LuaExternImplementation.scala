package dev.argon.plugins.lua

import dev.argon.esexpr.ESExprCodec

final case class LuaExternImplementation(importPath: String, memberName: String) derives ESExprCodec
