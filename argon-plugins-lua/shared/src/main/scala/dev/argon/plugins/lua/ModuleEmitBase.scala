package dev.argon.plugins.lua

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.{ErasedSignature, ErasedSignatureType}
import dev.argon.expr.{BinaryBuiltin, NullaryBuiltin}

trait ModuleEmitBase extends TubeEmitBase {
  val currentModule: VMModule
}
