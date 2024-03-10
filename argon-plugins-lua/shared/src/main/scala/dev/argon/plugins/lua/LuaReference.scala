package dev.argon.plugins.lua

import dev.argon.esexpr.ESExprCodec

enum LuaReference derives ESExprCodec {
  case Global(importTube: String, module: String, name: AST.Exp, signature: AST.Exp)
}
