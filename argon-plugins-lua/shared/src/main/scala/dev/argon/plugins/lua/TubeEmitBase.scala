package dev.argon.plugins.lua

trait TubeEmitBase extends EmitBase {
  val context: plugin.VMContextIncluding
  export context.*
  export context.implementations.*

  val currentTube: VMTube
}


