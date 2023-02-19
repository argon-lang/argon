package dev.argon.plugin.jsapi

import dev.argon.plugin.jsapi
import scalajs.js

trait TubeImporter extends js.Any {
  def getTube(tubeName: jsapi.proto.TubeName): js.Promise[SerializedTube]
}

trait TubeLoader[LibOptions] extends js.Any {
  def libOptionsDecoder: OptionDecoder[LibOptions]
  def load(tubeImporter: TubeImporter, libOptions: LibOptions): js.Promise[SerializedTube]
}

trait LoaderConsumer[A] extends js.Any {
  def consume[LibOptions](loader: TubeLoader[LibOptions]): A
}

trait LoaderFactory extends js.Any {
  def withLoader[A](consumer: LoaderConsumer[A]): A
}

trait PluginOperations extends js.Any

trait Plugin[Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation] extends js.Any {
  def optionCodec: OptionCodec[Options]
  def outputHandler: OutputHandler[Output]

  def emitTube(tube: SerializedTube): js.Promise[Output]

  def loadExternMethod(options: Options, id: String): js.Promise[ExternMethodImplementation | Null]
  def loadExternFunction(options: Options, id: String): js.Promise[ExternFunctionImplementation | Null]
  def loadExternClassConstructor(options: Options, id: String): js.Promise[ExternClassConstructorImplementation | Null]

  def tubeLoaders: js.Map[String, LoaderFactory]
}

trait PluginConsumer[A] extends js.Any {
  def consume[Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation]
  (plugin: Plugin[Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation])
  : A;
}


trait PluginFactory extends js.Any {
  def create[A](operations: PluginOperations, consumer: PluginConsumer[A]): A
}

