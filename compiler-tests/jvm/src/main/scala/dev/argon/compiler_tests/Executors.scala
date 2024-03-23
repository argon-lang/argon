package dev.argon.compiler_tests

import dev.argon.plugins.lua.{LuaPlugin, LuaEmitter}

def executors: Seq[TestExecutor] = Seq(
  LuaTestExecutor(),
)


