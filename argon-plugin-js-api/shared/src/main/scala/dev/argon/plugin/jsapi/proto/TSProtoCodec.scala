package dev.argon.plugin.jsapi.proto

import dev.argon.util.TypedArrayUtil
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
trait TSProtoCodec[T <: js.Any] extends js.Any {
  def encode(message: T, writer: ProtoWriter = js.native): ProtoWriter = js.native
  def decode(input: Uint8Array): T = js.native
}

@js.native
@JSImport("protobufjs", "Writer")
class ProtoWriter() extends js.Any {
  def finish(): Uint8Array = js.native
}


