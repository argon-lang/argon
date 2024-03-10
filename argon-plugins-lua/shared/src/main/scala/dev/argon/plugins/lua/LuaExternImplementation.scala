package dev.argon.plugins.lua

import dev.argon.esexpr.{ESExprCodec, constructor}

@constructor("lua-extern")
final case class LuaExternImplementation(importPath: String, memberName: String) derives ESExprCodec, CanEqual
