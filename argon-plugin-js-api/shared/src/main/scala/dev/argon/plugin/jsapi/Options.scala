package dev.argon.plugin.jsapi

import dev.argon.plugin.jsapi.proto.Toml
import scalajs.js

sealed trait OptionDecodeResult[A] extends js.Any
trait OptionDecodeResultValue[A] extends OptionDecodeResult[A] {
  val value: A
}

trait OptionDecodeResultError[A] extends OptionDecodeResult[A] {
  val errorMessage: String
}

trait OptionDecoder[Options] extends js.Any {
  def decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult[Options]
  def defaultValue: Options | Null
}

trait OptionCodec[Options] extends OptionDecoder[Options] {
  def encode(recorder: ResourceRecorder, value: Options): js.Promise[Toml]
  def skipForField(output: Options): Boolean
}

trait OutputInfo[Output] extends js.Any {
  def getValue(output: Output): FileSystemResource
}

trait OutputHandler[Output] extends js.Any {
  def options: js.Map[js.Array[String], OutputInfo[Output]]
}
