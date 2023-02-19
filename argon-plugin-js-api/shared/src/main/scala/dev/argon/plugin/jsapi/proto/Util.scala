package dev.argon.plugin.jsapi.proto

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

opaque type Toml <: js.Any = js.Any

@js.native
@JSImport("@argon-lang/plugin-util/proto/util.js", "Toml")
object Toml extends TSProtoCodec[Toml]
