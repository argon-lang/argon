package dev.argon.plugins.lua

import dev.argon.compiler.*
import dev.argon.util.{*, given}

trait TubeEmitBase {
  val plugin: LuaPlugin
  val context: plugin.VMContextIncluding
  export context.*
  export context.implementations.*

  val currentTube: VMTube

  protected def toArrayExp(values: Seq[AST.Exp]): AST.Exp =
    AST.TableConstructor(values.map(AST.Field.Positional.apply))

}


