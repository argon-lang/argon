package dev.argon.plugins.lua

import dev.argon.compiler.Context
import dev.argon.plugin.vm.*
import zio.ZEnvironment

trait TubeEmitBase extends EmitBase {
  val context: Context {
    val implementations: {
      type ExternFunctionImplementation <: ZEnvironment[LuaExternImplementation]
      type FunctionReference <: ZEnvironment[LuaReference]
      type RecordReference <: ZEnvironment[LuaReference]
    }
  }
  export context.{Comp, Env, Error}

  type Externs[E <: ExternalImplementation] = E match {
    case "function" => context.implementations.ExternFunctionImplementation
    case "function-reference" => context.implementations.FunctionReference
    case "record-reference" => context.implementations.RecordReference
  }

  val currentTube: VmTube[context.Env, context.Error, Externs]
}


