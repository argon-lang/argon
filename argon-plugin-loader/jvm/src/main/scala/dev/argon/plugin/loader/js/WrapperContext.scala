package dev.argon.plugin.loader.js

import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.{ArTubeC, TubeImporter, TubeName}
import dev.argon.compiler.{Context, HasContext}
import dev.argon.io.*
import dev.argon.options.{OptionCodec, OptionDecoder, OutputHandler, OutputInfo}
import dev.argon.plugin.*
import dev.argon.plugin.tube.*
import dev.argon.tube as t
import dev.argon.tube.ModulePath

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
//import dev.argon.plugin.loader.js.AsyncIterableTools.AsyncIterable
import dev.argon.util.toml.Toml
import dev.argon.util.{*, given}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.*
import zio.stream.*
import org.graalvm.polyglot.{Value, Context as JSContext}

import scala.reflect.TypeTest


final class WrapperContext[R, E >: InvalidTube](using runtime: Runtime[R], jsContext: JSContext) {
  final class WrappedError(private[WrapperContext] val value: Cause[E]) extends Exception

  private given ErrorWrapper[E, WrappedError] with
    override def wrap(error: Cause[E]): WrappedError = WrappedError(error)
    override def unwrap(ex: WrappedError): Cause[E] = ex.value
  end given

  private def execute(code: String)(args: Any*): Value =
    jsContext.eval("js", code).nn.execute(args *).nn


  private def getProperty[U: ValueDecoder](value: Value, name: String): U =
    summon[ValueDecoder[U]].decode(value.getMember(name).nn)

  private def invokeMember[U: ValueDecoder](value: Value, name: String)(): U =
    summon[ValueDecoder[U]].decode(value.invokeMember(name).nn)

  private def invokeMember[T0: ValueEncoder, U: ValueDecoder](value: Value, name: String)(arg0: T0): U =
    summon[ValueDecoder[U]].decode(value.invokeMember(name, ValueEncoder[T0].encode(arg0)).nn)

  private def invokeMember[T0: ValueEncoder, T1: ValueEncoder, U: ValueDecoder](value: Value, name: String)(arg0: T0, arg1: T1): U =
    summon[ValueDecoder[U]].decode(value.invokeMember(name, ValueEncoder[T0].encode(arg0), ValueEncoder[T1].encode(arg1)).nn)

  private final class JSClassInfo[T](classValue: Value, nativeValueSymbol: Value) {

    final case class WrappedInstance(value: T)

    private given ValueEncoder[WrappedInstance] with
      override def encode(value: WrappedInstance): Value = jsContext.asValue(value).nn
    end given

    private given ValueDecoder[WrappedInstance] with
      override def decode(value: Value): WrappedInstance = value.as(classOf[WrappedInstance]).nn
    end given

    def newInstance(nativeValue: T): Value =
      execute("(value, f) => new f(value)")(WrappedInstance(nativeValue), classValue)

    def tryGetInstance(value: Value): Option[T] =
      val wrappedInstance = execute("(o, sym) => o[sym]")(value, nativeValueSymbol)
      if wrappedInstance.isNull then
        None
      else
        Some(wrappedInstance.asHostObject[AnyRef].asInstanceOf[T])
    end tryGetInstance

    def createMethod[U: ValueEncoder](name: String)(method: T => U): this.type =
      val func = ValueEncoder[WrappedInstance => U].encode(inst => method(inst.value))
      execute("(sym, classValue, name, method) => classValue.prototype[name] = function(...args) { return method(this[sym], ...args); };")(nativeValueSymbol, classValue, name, func)
      this
    end createMethod

    def createMethod[T1: ValueDecoder, U: ValueEncoder](name: String)(method: (T, T1) => U): this.type =
      val func = ValueEncoder[(WrappedInstance, T1) => U].encode((inst, arg1) => method(inst.value, arg1))
      execute("(sym, classValue, name, method) => classValue.prototype[name] = function(...args) { return method(this[sym], ...args); };")(nativeValueSymbol, classValue, name, func)
      this
    end createMethod

    def createProp[U: ValueEncoder](name: String)(method: T => U): this.type =
      val func = ValueEncoder[WrappedInstance => U].encode(inst => method(inst.value))
      execute("(sym, classValue, name, method) => Object.defineProperty(classValue.prototype, name, { get() { return method(this[sym]); } });")(nativeValueSymbol, classValue, name, func)
      this
    end createProp
  }


  private def createClass[T](name: String): JSClassInfo[T] =
    val sym = execute("Symbol")()
    val classValue = execute(s"sym => function $name(nativeValue) { this[sym] = nativeValue; }")(sym)
    JSClassInfo(classValue, sym)
  end createClass

  private class JSInterfaceEncoder[T](classValue: => JSClassInfo[T]) extends ValueEncoder[T] {
    override def encode(value: T): Value = classValue.newInstance(value)
  }

  private final class JSInterfaceDecoder[T](classValue: => JSClassInfo[T], unwrapInstance: Value => T) extends ValueDecoder[T] {
    override def decode(value: Value): T =
      classValue.tryGetInstance(value) match
        case Some(value) => value
        case None => unwrapInstance(value)
  }

  private final class JSInterfaceDecoderUnwrapOnly[T](unwrapInstance: Value => T) extends ValueDecoder[T] {
    override def decode(value: Value): T = unwrapInstance(value)
  }


  private given ValueEncoder[Operations] = JSInterfaceEncoder(operationsClass)
  private given ValueEncoder[BinaryResource[R, E]] = JSInterfaceEncoder(wrapBinaryResourceClass)
  private given ValueDecoder[BinaryResource[R, E]] = JSInterfaceDecoder(wrapBinaryResourceClass, UnwrapBinaryResource(_))
  private given ValueEncoder[DirectoryResource[R, E, BinaryResource]] = JSInterfaceEncoder(wrapDirectoryResourceClass)
  private given ValueDecoder[DirectoryResource[R, E, BinaryResource]] = JSInterfaceDecoder(wrapDirectoryResourceClass, UnwrapDirectoryResource(_))
  private given ValueEncoder[ResourceFactory[R, E]] = JSInterfaceEncoder(wrapResourceFactoryClass)
  private given ValueEncoder[ResourceRecorder[R, E]] = JSInterfaceEncoder(wrapResourceRecorderClass)
  private given [A: ValueDecoder]: ValueDecoder[OptionDecoder[R, E, A]] = JSInterfaceDecoderUnwrapOnly(UnwrapOptionDecoder(_))
  private given [A: ValueDecoder: ValueEncoder]: ValueDecoder[OptionCodec[R, E, A]] = JSInterfaceDecoderUnwrapOnly(UnwrapOptionCodec(_))
  private given[A: ValueEncoder]: ValueDecoder[OutputInfo[R, E, A]] = JSInterfaceDecoderUnwrapOnly(UnwrapOutputInfo(_))
  private given[A: ValueEncoder]: ValueDecoder[OutputHandler[R, E, A]] = JSInterfaceDecoderUnwrapOnly(UnwrapOutputHandler(_))
  private given ValueEncoder[SerializedTube[R, E]] = JSInterfaceEncoder(wrapSerializedTube)
  private given ValueDecoder[SerializedTube[R, E]] = JSInterfaceDecoder(wrapSerializedTube, UnwrapSerializedTube(_))
  private given[ContextOptions, LibOptions: ValueDecoder: ValueEncoder]: ValueDecoder[TubeLoader.Aux[R, E, ContextOptions, LibOptions]] = JSInterfaceDecoderUnwrapOnly(UnwrapTubeLoader(_))
  private given[
    JSOptions: ValueEncoder: ValueDecoder,
    JSOutput: ValueEncoder: ValueDecoder,
    ExternMethodImpl: ValueDecoder,
    ExternFunctionImpl: ValueDecoder,
    ExternClassConstructorImpl: ValueDecoder
  ]: ValueDecoder[Plugin[R, E] {
    type Options = JSOptions
    type Output = JSOutput
    type ExternMethodImplementation = ExternMethodImpl
    type ExternFunctionImplementation = ExternFunctionImpl
    type ExternClassConstructorImplementation = ExternClassConstructorImpl
  }] = JSInterfaceDecoderUnwrapOnly(UnwrapPlugin(_))

  private given ValueEncoder[FileSystemResource[R, E]] with
    override def encode(value: FileSystemResource[R, E]): Value =
      value match {
        case value: BinaryResource[R, E] => ValueEncoder[BinaryResource[R, E]].encode(value)
        case value: DirectoryResource[R, E, BinaryResource] => ValueEncoder[DirectoryResource[R, E, BinaryResource]].encode(value)
      }
  end given

  private given ValueDecoder[FileSystemResource[R, E]] with
    override def decode(value: Value): FileSystemResource[R, E] =
      val resType = value.getMember("resourceType").nn.asString().nn
      if resType == "directory" then
        ValueDecoder[DirectoryResource[R, E, BinaryResource]].decode(value)
      else if resType == "binary" then
        ValueDecoder[BinaryResource[R, E]].decode(value)
      else
        throw new RuntimeException("Unexpected resourceType")
    end decode
  end given

  private given ValueEncoder[DirectoryEntry[R, E, BinaryResource]] with
    override def encode(value: DirectoryEntry[R, E, BinaryResource]): Value =
      value match {
        case DirectoryEntry.Subdirectory(name, resource) =>
          execute("(name, resource) => ({ entryType: 'subdirectory', name, resource })")(name, wrapDirectoryResourceClass.newInstance(resource))

        case DirectoryEntry.File(name, resource) =>
          execute("(name, resource) => ({ entryType: 'file', name, resource })")(name, wrapBinaryResourceClass.newInstance(resource))
      }
  end given

  private given ValueDecoder[DirectoryEntry[R, E, BinaryResource]] with
    override def decode(value: Value): DirectoryEntry[R, E, BinaryResource] =
      val entryType = value.getMember("entryType").nn.asString().nn
      if entryType == "subdirectory" then
        val name = getProperty[String](value, "name")
        val res = getProperty[DirectoryResource[R, E, BinaryResource]](value, "name")
        DirectoryEntry.Subdirectory(name, res)
      else if entryType == "file" then
        val name = getProperty[String](value, "name")
        val res = getProperty[BinaryResource[R, E]](value, "name")
        DirectoryEntry.File(name, res)
      else
        throw new RuntimeException("Unexpected entryType")
      end if
    end decode
  end given


  given[A <: GeneratedMessage]: ValueEncoder[A] with
    override def encode(value: A): Value = ProtoConverterJS.convertPValueToJS(value.toPMessage)
  end given

  given[A <: GeneratedMessage](using companion: GeneratedMessageCompanion[A]): ValueDecoder[A] with
    override def decode(value: Value): A =
      val pmsg = ProtoConverterJS.convertJSToPMessage(value, companion.scalaDescriptor)
      companion.messageReads.read(pmsg)
    end decode
  end given

  final class Operations

  private lazy val operationsClass = createClass[Operations]("Operations")

  private lazy val wrapResourceFactoryClass = createClass[ResourceFactory[R, E]]("WrapResourceFactory")
    .createMethod[String, Value]("binaryResource") { (inst, name) =>
      wrapBinaryResourceClass.newInstance(inst.binaryResource(name))
    }
    .createMethod[String, Value]("directoryResource") { (inst, name) =>
      wrapDirectoryResourceClass.newInstance(inst.directoryResource(name))
    }

  private lazy val wrapResourceRecorderClass: JSClassInfo[ResourceRecorder[R, E]] = createClass[ResourceRecorder[R, E]]("WrapResourceRecorder")
    .createMethod[BinaryResource[R, E], JSPromise[String]]("recordBinaryResource") { (inst, resource) =>
      JSPromiseUtil.runEffectToPromise(inst.recordBinaryResource(resource))
    }
    .createMethod[DirectoryResource[R, E, BinaryResource], JSPromise[String]]("recordDirectoryResource") { (inst, resource) =>
      JSPromiseUtil.runEffectToPromise(inst.recordDirectoryResource(resource))
    }

  private lazy val wrapBinaryResourceClass = createClass[BinaryResource[R, E]]("WrapBinaryResource")
    .createProp["binary"]("resourceType") { _ => "binary" }
    .createProp[String | Null]("fileName") { res => res.fileName.orNull }
    .createMethod[AsyncIterable[Uint8Array]]("asAsyncIterable") { res =>
      AsyncIterableTools.zstreamToAsyncIterable(
        res.asBytes.chunks.map(TypedArrayUtil.fromByteChunk)
      )
    }

  private lazy val wrapDirectoryResourceClass: JSClassInfo[DirectoryResource[R, E, BinaryResource]] = createClass[DirectoryResource[R, E, BinaryResource]]("WrapDirectoryResource")
    .createProp["directory"]("resourceType") { _ => "directory" }
    .createProp[String | Null]("fileName") { res => res.fileName.orNull }
    .createMethod[AsyncIterable[DirectoryEntry[R, E, BinaryResource]]]("contents") { res =>
      AsyncIterableTools.zstreamToAsyncIterable(res.contents)
    }


  private def wrapFileSystemResource(res: FileSystemResource[R, E]): Value =
    res match {
      case res: BinaryResource[R, E] => wrapBinaryResourceClass.newInstance(res)
      case res: DirectoryResource[R, E, BinaryResource] => wrapDirectoryResourceClass.newInstance(res)
    }

  private lazy val wrapSerializedTube = createClass[SerializedTube[R, E]]("WrapSerializedTube")
    .createMethod[JSPromise[t.TubeFormatVersion]]("version") { tube => JSPromiseUtil.runEffectToPromise(tube.version) }
    .createMethod[JSPromise[t.Metadata]]("metadata") { tube => JSPromiseUtil.runEffectToPromise(tube.metadata) }
    .createMethod[String, JSPromise[FileSystemResource[R, E]]]("getResource") { (tube, name) => JSPromiseUtil.runEffectToPromise(tube.getResource(name)) }
    .createMethod[t.ModulePath, JSPromise[t.ModuleDefinition]]("getModule") { (tube, path) => JSPromiseUtil.runEffectToPromise(tube.getModule(path)) }
    .createMethod[BigInt, JSPromise[t.ClassDefinition]]("getClass") { (tube, id) => JSPromiseUtil.runEffectToPromise(tube.getClass(id)) }
    .createMethod[BigInt, JSPromise[t.TraitDefinition]]("getTrait") { (tube, id) => JSPromiseUtil.runEffectToPromise(tube.getTrait(id)) }
    .createMethod[BigInt, JSPromise[t.FunctionDefinition]]("getFunction") { (tube, id) => JSPromiseUtil.runEffectToPromise(tube.getFunction(id)) }
    .createMethod[BigInt, JSPromise[t.MethodDefinition]]("getMethod") { (tube, id) => JSPromiseUtil.runEffectToPromise(tube.getMethod(id)) }
    .createMethod[BigInt, JSPromise[t.ClassConstructorDefinition]]("getClassConstructor") { (tube, id) => JSPromiseUtil.runEffectToPromise(tube.getClassConstructor(id)) }


  private def wrapTubeImporter(context: Context { type Env = R; type Error = E }) = createClass[TubeImporter & HasContext[context.type]]("TubeImporter")
    .createMethod[t.TubeName, JSPromise[SerializedTube[R, E]]]("getTube") { (importer, tubeName) =>
      JSPromiseUtil.runEffectToPromise[R, E, WrappedError, SerializedTube[R, E]](
        importer.getTube(TubeName(NonEmptyList.fromList(tubeName.name.toList).get))
          .flatMap(TubeSerializer.ofInterface(context)(_))
      )
    }


  private enum OptionDecodeResult[A] {
    case Success(value: A)
    case Failure(message: String)

    def toEither: Either[String, A] =
      this match {
        case Success(value) => Right(value)
        case Failure(message) => Left(message)
      }
  }

  private given [A: ValueDecoder]: ValueDecoder[OptionDecodeResult[A]] with
    override def decode(value: Value): OptionDecodeResult[A] =
      if value.hasMember("value") then
        OptionDecodeResult.Success(getProperty[A](value, "value"))
      else
        OptionDecodeResult.Failure(getProperty[String](value, "errorMessage"))
  end given


  private class UnwrapOptionDecoder[A: ValueDecoder](optDecoder: Value) extends OptionDecoder[R, E, A] {


    override final def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A] =
      invokeMember[ResourceFactory[R, E], t.Toml, OptionDecodeResult[A]](optDecoder, "decode")(resFactory, t.TomlConverter.encodeToml(value))
        .toEither

    override final def defaultValue: Option[A] =
      Nullable(getProperty[A | Null](optDecoder, "defaultValue")(using valueDecoderNullable[A])).toOption
  }


  private final class UnwrapOptionCodec[A: ValueEncoder: ValueDecoder](optCodec: Value) extends UnwrapOptionDecoder[A](optCodec) with OptionCodec[R, E, A] {
    override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Toml] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[ResourceRecorder[R, E], A, JSPromise[t.Toml]](optCodec, "encode")(recorder, value)
      )
        .map(t.TomlConverter.decodeToml)
  }

  final class UnwrapOutputHandler[A: ValueEncoder](outputHandler: Value) extends OutputHandler[R, E, A] {
    override lazy val options: Map[Seq[String], OutputInfo[R, E, A]] =
      JSMap.toScala(getProperty[JSMap[String, OutputInfo[R, E, A]]](outputHandler, "options"))
        .map { (key, value) =>
          val newKey = key
            .split("\\.").nn
            .iterator
            .map { keyPart =>
              URLDecoder.decode(keyPart, StandardCharsets.UTF_8).nn
            }
            .toSeq

          newKey -> value
        }
  }

  final class UnwrapOutputInfo[A: ValueEncoder](outputInfo: Value) extends OutputInfo[R, E, A] {
    override def getValue(options: A): FileSystemResource[R, E] =
      invokeMember[A, FileSystemResource[R, E]](outputInfo, "getValue")(options)
  }


  final class UnwrapBinaryResource(value: Value) extends BinaryResource[R, E] {
    override def fileName: Option[String] =
      Nullable(getProperty[String | Null](value, "fileName")(using valueDecoderNullable[String])).toOption

    override def asBytes: ZStream[R, E, Byte] =
      AsyncIterableTools.asyncIterableToZStream(invokeMember[AsyncIterable[Uint8Array]](value, "asAsyncIterable")())
        .map(TypedArrayUtil.toByteChunk)
        .flattenChunks

  }

  final class UnwrapDirectoryResource(value: Value) extends DirectoryResource[R, E, BinaryResource] {
    override def fileName: Option[String] =
      Nullable(ValueDecoder[String | Null].decode(value.getMember("fileName").nn)).toOption

    override def contents: ZStream[R, E, DirectoryEntry[R, E, BinaryResource]] =
      AsyncIterableTools.asyncIterableToZStream(
        invokeMember[AsyncIterable[DirectoryEntry[R, E, BinaryResource]]](value, "contents")()
      )
  }

  final class UnwrapResourceFactory(value: Value) extends ResourceFactory[R, E] {
    override def directoryResource(name: String): DirectoryResource[R, E, BinaryResource] =
      invokeMember[String, DirectoryResource[R, E, BinaryResource]](value, "directoryResource")(name)

    override def binaryResource(name: String): BinaryResource[R, E] =
      invokeMember[String, BinaryResource[R, E]](value, "binaryResource")(name)
  }


  final class UnwrapTubeLoader[ContextOptions, JSLibOptions: ValueDecoder: ValueEncoder]
  (tubeLoader: Value)
    extends TubeLoader[R, E, ContextOptions] {

    override type LibOptions = JSLibOptions
    override def libOptionDecoder(using contextOptionsDecoder: OptionDecoder[R, E, ContextOptions]): OptionDecoder[R, E, JSLibOptions] =
      invokeMember[OptionDecoder[R, E, JSLibOptions]](tubeLoader, "libOptionsDecoder")()

    override def load
    (ctx: Context { type Env = R; type Error = E; type Options = ContextOptions })
    (tubeImporter2: TubeImporter & HasContext[ctx.type])
    (libOptions: JSLibOptions)
    : ZIO[R & Scope, E, ArTubeC & HasContext[ctx.type]] =
      given ValueEncoder[TubeImporter & HasContext[ctx.type]] = JSInterfaceEncoder(wrapTubeImporter(ctx))

      JSPromiseUtil.promiseToEffect(
        invokeMember[TubeImporter & HasContext[ctx.type], JSLibOptions, JSPromise[Value]](tubeLoader, "load")(tubeImporter2, libOptions)
      )
        .withFinalizer { tube =>
          JSPromiseUtil.fromPromiseJS(
            invokeMember[JSPromise[Unit]](tube, "close")()
          ).orDie
        }
        .flatMap { tubeValue =>
          new TubeReaderFactory {
            override val context: ctx.type = ctx
            override protected val serialized: SerializedTube[context.Env, context.Error] = ValueDecoder[SerializedTube[R, E]].decode(tubeValue)
            override protected val tubeImporter: TubeImporter & HasContext[context.type] = tubeImporter2
          }.create
        }
        .flatMap(_.asTube)

  }

  private final class UnwrapSerializedTube(tubeValue: Value) extends SerializedTube[R, E] {

    override def version: ZIO[R, E, t.TubeFormatVersion] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[JSPromise[t.TubeFormatVersion]](tubeValue, "version")()
      )

    override def metadata: ZIO[R, E, t.Metadata] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[JSPromise[t.Metadata]](tubeValue, "metadata")()
      )

    override def getResource(id: String): ZIO[R, E, FileSystemResource[R, E]] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[JSPromise[FileSystemResource[R, E]]](tubeValue, "getResource")()
      )

    override def getModule(modulePath: t.ModulePath): ZIO[R, E, t.ModuleDefinition] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[t.ModulePath, JSPromise[t.ModuleDefinition]](tubeValue, "getModule")(modulePath)
      )

    override def getClass(id: BigInt): ZIO[R, E, t.ClassDefinition] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[BigInt, JSPromise[t.ClassDefinition]](tubeValue, "getClass")(id)
      )

    override def getTrait(id: BigInt): ZIO[R, E, t.TraitDefinition] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[BigInt, JSPromise[t.TraitDefinition]](tubeValue, "getTrait")(id)
      )

    override def getFunction(id: BigInt): ZIO[R, E, t.FunctionDefinition] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[BigInt, JSPromise[t.FunctionDefinition]](tubeValue, "getFunction")(id)
      )

    override def getMethod(id: BigInt): ZIO[R, E, t.MethodDefinition] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[BigInt, JSPromise[t.MethodDefinition]](tubeValue, "getMethod")(id)
      )

    override def getClassConstructor(id: BigInt): ZIO[R, E, t.ClassConstructorDefinition] =
      JSPromiseUtil.promiseToEffect(
        invokeMember[BigInt, JSPromise[t.ClassConstructorDefinition]](tubeValue, "getClassConstructor")(id)
      )
  }

  private final class UnwrapPlugin[JSOptions: ValueDecoder: ValueEncoder, JSOutput: ValueDecoder: ValueEncoder, JSExternMethod: ValueDecoder, JSExternFunction: ValueDecoder, JSExternClassConstructor: ValueDecoder]
  (pluginValue: Value)
    extends Plugin[R, E] {

    override type Options = JSOptions
    override type Output = JSOutput

    override def optionCodec: OptionCodec[R, E, JSOptions] =
      getProperty[OptionCodec[R, E, JSOptions]](pluginValue, "optionCodec")

    override def outputHandler: OutputHandler[R, E, Output] =
      getProperty[OutputHandler[R, E, Output]](pluginValue, "outputHandler")


    override type ExternMethodImplementation = JSExternMethod
    override type ExternFunctionImplementation = JSExternFunction
    override type ExternClassConstructorImplementation = JSExternClassConstructor


    override def emitTube
    (context: Context {type Env = R; type Error = E})
    (adapter: PluginContextAdapter.Aux[context.type, UnwrapPlugin.this.type])
    (tube: ArTubeC & HasContext[context.type] & HasImplementation[true]): context.Comp[JSOutput] =
      TubeSerializer.ofImplementation(context)(tube).flatMap { serializedTube =>
        JSPromiseUtil.promiseToEffect(
          invokeMember[SerializedTube[R, E], JSPromise[JSOutput]](pluginValue, "emitTube")(serializedTube)
        )
      }

    override def loadExternMethod(options: JSOptions)(id: String): ZIO[R, E, Option[JSExternMethod]] =
      JSPromiseUtil.promiseToEffect {
        invokeMember[JSOptions, String, JSPromise[JSExternMethod | Null]](pluginValue, "loadExternMethod")(options, id)
      }.map { extern => Nullable(extern).toOption }

    override def loadExternFunction(options: JSOptions)(id: String): ZIO[R, E, Option[JSExternFunction]] =
      JSPromiseUtil.promiseToEffect {
        invokeMember[JSOptions, String, JSPromise[JSExternFunction | Null]](pluginValue, "loadExternFunction")(options, id)
      }.map { extern => Nullable(extern).toOption }

    override def loadExternClassConstructor(options: JSOptions)(id: String): ZIO[R, E, Option[JSExternClassConstructor]] =
      JSPromiseUtil.promiseToEffect {
        invokeMember[JSOptions, String, JSPromise[JSExternClassConstructor | Null]](pluginValue, "loadExternClassConstructor")(options, id)
      }.map { extern => Nullable(extern).toOption }

    override def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, ContextOptions]] =
      JSMap.toScala(invokeMember[JSMap[String, Value]](pluginValue, "tubeLoaders")())
        .view
        .mapValues { loaderFactory =>
          val consumer = execute("() => ({ consume(loader) { return loader; } })")()
          invokeMember[Value, TubeLoader.Aux[R, E, ContextOptions, Value]](loaderFactory, "withLoader")(consumer)
        }
        .toMap
  }


  def createPlugin(factory: Value)(operations: Operations): Plugin[R, E] =
    val consumer = execute("() => ({ consume(plugin) { return plugin; } })")()
    type PluginWithValues = Plugin[R, E] {
      type Options = Value
      type Output = Value
      type ExternMethodImplementation = Value
      type ExternFunctionImplementation = Value
      type ExternClassConstructorImplementation = Value
    }

    invokeMember[Operations, Value, PluginWithValues](factory, "create")(operations, consumer)
  end createPlugin

}
