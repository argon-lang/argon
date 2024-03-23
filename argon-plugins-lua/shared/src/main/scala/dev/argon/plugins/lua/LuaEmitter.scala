package dev.argon.plugins.lua

import dev.argon.compiler.Context
import dev.argon.plugin.{PluginEnv, PluginError, TubeEmitter}

trait LuaEmitter[Ctx <: Context { type Env <: PluginEnv; type Error >: PluginError }] extends TubeEmitter[Ctx] {
  type OutputOptions[E >: PluginError] = LuaOutputOptions
  type Output[E >: PluginError] = LuaOutput[E]
}
