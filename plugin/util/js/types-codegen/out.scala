package dev.argon.plugin.jsapi
type OptionDecodeResult[A] = _root_.dev.argon.plugin.jsapi.OptionDecodeSuccess[A] | _root_.dev.argon.plugin.jsapi.OptionDecodeFailure
trait OptionDecodeSuccess[A] {
	val ok: true
	val value: A
}
trait OptionDecodeFailure {
	val ok: false
	val errorMessage: _root_.java.lang.String
}
trait OptionDecoder[Options] {
	def decode(resourceFactory: _root_.dev.argon.plugin.jsapi.ResourceFactory, value: _root_.dev.argon.plugin.jsapi.tube.Toml): _root_.dev.argon.plugin.jsapi.OptionDecodeResult[Options]
}
trait OptionCodec[Options] extends _root_.dev.argon.plugin.jsapi.OptionDecoder[Options] {
	def encode(recorder: _root_.dev.argon.plugin.jsapi.ResourceRecorder, value: Options): _root_.scala.scalajs.js.Promise[_root_.dev.argon.plugin.jsapi.tube.Toml]
}
trait OutputInfo[Output] {
	def getValue(output: Output): _root_.dev.argon.plugin.jsapi.FileSystemResource
}
trait OutputHandler[Output] {
	def options: _root_.scala.scalajs.js.Map[_root_.java.lang.String, _root_.dev.argon.plugin.jsapi.OutputInfo[Output]]
}
