package dev.argon.plugin.loader.js

import dev.argon.compiler.{Context, HasContext}
import dev.argon.compiler.tube.{ArTubeC, TubeImporter, TubeName}
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.io.*
import dev.argon.options.{OptionCodec, OptionDecoder, OutputHandler, OutputInfo}
import zio.*
import zio.stream.*
import dev.argon.plugin.*
import dev.argon.plugin.tube.{InvalidTube, SerializedTube, TubeReaderBase, TubeReaderFactory, TubeSerializer}
import dev.argon.tube as t
import dev.argon.util.AsyncIterableTools.AsyncIterable
import dev.argon.util.{AsyncIterableTools, ErrorWrapper, JSPromiseUtil, NonEmptyList, Nullable, TypedArrayUtil}
import dev.argon.util.toml.Toml

import scala.reflect.TypeTest
import scalajs.js
import org.scalajs.dom.ReadableStream
import dev.argon.plugin.jsapi
import dev.argon.plugin.jsapi.LoaderConsumer
import dev.argon.tube.ModulePath
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

import scala.scalajs.js.typedarray.{Int8Array, Uint8Array, byteArray2Int8Array}


final class WrapperContext[R, E >: InvalidTube](using runtime: Runtime[R]) {
  final class WrappedError(private[WrapperContext] val value: Cause[E]) extends Exception

  private given ErrorWrapper[E, WrappedError] with
    override def wrap(error: Cause[E]): WrappedError = WrappedError(error)
    override def unwrap(ex: WrappedError): Cause[E] = ex.value
  end given


  final class Operations extends js.Object with jsapi.PluginOperations

  
  final class WrapResourceFactory(val inner: ResourceFactory[R, E]) extends js.Object with jsapi.ResourceFactory {
    override def binaryResource(name: String): jsapi.BinaryResource =
      WrapBinaryResource(inner.binaryResource(name))

    override def directoryResource(name: String): jsapi.DirectoryResource[jsapi.BinaryResource] =
      WrapDirectoryResource(inner.directoryResource(name))
  }

  final class WrapResourceRecorder(val inner: ResourceRecorder[R, E]) extends js.Object with jsapi.ResourceRecorder {
    override def recordBinaryResource(resource: jsapi.BinaryResource): js.Promise[String] =
      JSPromiseUtil.runEffectToPromise(inner.recordBinaryResource(UnwrapBinaryResource(resource)))

    override def recordDirectoryResource(resource: jsapi.DirectoryResource[jsapi.BinaryResource]): js.Promise[String] =
      JSPromiseUtil.runEffectToPromise(inner.recordDirectoryResource(UnwrapDirectoryResource(resource)))
  }


  final class WrapBinaryResource(res: BinaryResource[R, E]) extends js.Object with jsapi.BinaryResource {
    override val resourceType: "binary" = "binary"

    override def fileName: String | Null =
      res.fileName.orNull

    override def asAsyncIterable(): AsyncIterable[Uint8Array] =
      AsyncIterableTools.zstreamToAsyncIterable(
        res.asBytes
          .chunks
          .map { chunk => new Uint8Array(byteArray2Int8Array(chunk.toArray).buffer) }
      )
  }

  final class WrapDirectoryResource(res: DirectoryResource[R, E, BinaryResource]) extends js.Object with jsapi.DirectoryResource[jsapi.BinaryResource] {
    override val resourceType: "directory" = "directory"

    override def fileName: String | Null =
      res.fileName.orNull

    override def contents(): AsyncIterable[jsapi.DirectoryEntry[jsapi.BinaryResource]] =
      AsyncIterableTools.zstreamToAsyncIterable(
        res.contents
          .map[jsapi.DirectoryEntry[jsapi.BinaryResource]] {
            case DirectoryEntry.Subdirectory(name2, resource2) =>
              new js.Object with jsapi.DirectoryEntry.Subdirectory[jsapi.BinaryResource] {
                override val entryType: "subdirectory" = "subdirectory"
                override val name: String = name
                override val resource: jsapi.DirectoryResource[jsapi.BinaryResource] =
                  WrapDirectoryResource(resource2)
              }

            case DirectoryEntry.File(name2, resource2) =>

              new js.Object with jsapi.DirectoryEntry.File[jsapi.BinaryResource] {
                override val entryType: "file" = "file"
                override val name: String = name2

                override val resource: jsapi.BinaryResource =
                  WrapBinaryResource(resource2)
              }
          }
      )

  }

  def wrapFileSystemResource(res: FileSystemResource[R, E]): jsapi.FileSystemResource =
    res match {
      case res: BinaryResource[R, E] => WrapBinaryResource(res)
      case res: DirectoryResource[R, E, BinaryResource] => WrapDirectoryResource(res)
    }

  private def convertProtoToJS[A <: GeneratedMessage, B <: js.Any](companion: GeneratedMessageCompanion[A], jsCodec: jsapi.proto.TSProtoCodec[B])(a: A): B =
    jsCodec.decode(TypedArrayUtil.fromByteArray(companion.toByteArray(a)))

  private def convertProtoFromJS[A <: GeneratedMessage, B <: js.Any](companion: GeneratedMessageCompanion[A], jsCodec: jsapi.proto.TSProtoCodec[B])(b: B): A =
    companion.parseFrom(TypedArrayUtil.toByteArray(jsCodec.encode(b).finish()))

  final class WrapSerializedTube(val inner: SerializedTube[R, E]) extends jsapi.SerializedTube {

    private def wrapProtoPromise[A <: GeneratedMessage, B <: js.Any](companion: GeneratedMessageCompanion[A], jsCodec: jsapi.proto.TSProtoCodec[B])(a: ZIO[R, E, A]): js.Promise[B] =
      JSPromiseUtil.runEffectToPromise(a.map(convertProtoToJS(companion, jsCodec)))

    override def version(): js.Promise[jsapi.proto.TubeFormatVersion] =
      wrapProtoPromise(t.TubeFormatVersion, jsapi.proto.TubeFormatVersion)(inner.version)

    override def metadata(): js.Promise[jsapi.proto.Metadata] =
      wrapProtoPromise(t.Metadata, jsapi.proto.Metadata)(inner.metadata)

    override def getResource(id: String): js.Promise[jsapi.FileSystemResource] =
      JSPromiseUtil.runEffectToPromise(
        inner.getResource(id)
          .map(wrapFileSystemResource)
      )

    override def getModule(modulePath: jsapi.proto.ModulePath): js.Promise[jsapi.proto.ModuleDefinition] =
      val path = ModulePath(convertProtoFromJS(t.ModulePath, jsapi.proto.ModulePath)(modulePath).name)
      wrapProtoPromise(t.ModuleDefinition, jsapi.proto.ModuleDefinition)(inner.getModule(path))
    end getModule

    override def getClass(id: js.BigInt): js.Promise[jsapi.proto.ClassDefinition] =
      wrapProtoPromise(t.ClassDefinition, jsapi.proto.ClassDefinition)(inner.getClass(BigInt(id.toString())))

    override def getTrait(id: js.BigInt): js.Promise[jsapi.proto.TraitDefinition] =
      wrapProtoPromise(t.TraitDefinition, jsapi.proto.TraitDefinition)(inner.getTrait(BigInt(id.toString())))

    override def getFunction(id: js.BigInt): js.Promise[jsapi.proto.FunctionDefinition] =
      wrapProtoPromise(t.FunctionDefinition, jsapi.proto.FunctionDefinition)(inner.getFunction(BigInt(id.toString())))

    override def getMethod(id: js.BigInt): js.Promise[jsapi.proto.MethodDefinition] =
      wrapProtoPromise(t.MethodDefinition, jsapi.proto.MethodDefinition)(inner.getMethod(BigInt(id.toString())))

    override def getClassConstructor(id: js.BigInt): js.Promise[jsapi.proto.ClassConstructorDefinition] =
      wrapProtoPromise(t.ClassConstructorDefinition, jsapi.proto.ClassConstructorDefinition)(inner.getClassConstructor(BigInt(id.toString())))

    override def close(): js.Promise[Unit] = js.Promise.resolve(())
  }

  private final class WrapTubeImporter(context: Context { type Env = R; type Error = E })(inner: TubeImporter & HasContext[context.type]) extends js.Object with jsapi.TubeImporter {
    override def getTube(tubeName: jsapi.proto.TubeName): js.Promise[jsapi.SerializedTube] =
      val tubeNameScala = convertProtoFromJS(t.TubeName, jsapi.proto.TubeName)(tubeName)
      val tubeNameAr = TubeName(NonEmptyList.fromList(tubeNameScala.name.toList).get)
      JSPromiseUtil.runEffectToPromise(
        inner.getTube(tubeNameAr)
          .flatMap(TubeSerializer.ofInterface(context)(_))
          .map(WrapSerializedTube(_))
      )
    end getTube
  }



  class UnwrapOptionDecoder[A](jOptionDecoder: jsapi.OptionDecoder[A]) extends OptionDecoder[R, E, A] {
    override final def decode(resFactory: ResourceFactory[R, E])(value: Toml): IO[String, A] =
      val value2 = convertProtoToJS(t.Toml, jsapi.proto.Toml)(t.TomlConverter.encodeToml(value))
      ZIO.succeed {
        jOptionDecoder.decode(WrapResourceFactory(resFactory), value2)
      }
        .flatMap { res =>
          if res.asInstanceOf[js.Dictionary[?]].contains("value") then
            ZIO.succeed(res.asInstanceOf[jsapi.OptionDecodeResultValue[A]].value)
          else
            ZIO.fail(res.asInstanceOf[jsapi.OptionDecodeResultError[A]].errorMessage)
        }
    end decode

    override final def defaultValue: Option[A] = Nullable(jOptionDecoder.defaultValue).toOption
  }

  object UnwrapOptionDecoder {
    def apply[A](jOptionDecoder: jsapi.OptionDecoder[A]): OptionDecoder[R, E, A] =
      new UnwrapOptionDecoder[A](jOptionDecoder)
  }

  final class UnwrapOptionCodec[A](jOptionCodec: jsapi.OptionCodec[A]) extends UnwrapOptionDecoder[A](jOptionCodec) with OptionCodec[R, E, A] {
    override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Toml] =
      JSPromiseUtil.promiseToEffect {
        jOptionCodec.encode(WrapResourceRecorder(recorder), value)
      }
        .flatMap { toml =>
          ZIO.fromOption(t.TomlConverter.decodeToml(convertProtoFromJS(t.Toml, jsapi.proto.Toml)(toml)))
            .mapError { _ => InvalidTube("Invalid TOML") }
        }

    override def skipForField(value: A): Boolean = jOptionCodec.skipForField(value)
  }

  final class UnwrapOutputHandler[A](jOptionCodec: jsapi.OutputHandler[A]) extends OutputHandler[R, E, A] {
    override lazy val options: Map[Seq[String], OutputInfo[R, E, A]] =
      jOptionCodec.options
        .view
        .map {
          case (optPath, outputInfo) =>
            (optPath.toSeq, UnwrapOutputInfo(outputInfo))
        }
        .toMap
  }

  final class UnwrapOutputInfo[A](jOptionCodec: jsapi.OutputInfo[A]) extends OutputInfo[R, E, A] {
    override def getValue(options: A): FileSystemResource[R, E] =
      unwrapFileSystemResource(jOptionCodec.getValue(options))
  }


  def unwrapFileSystemResource(jsRes: jsapi.FileSystemResource): FileSystemResource[R, E] =
    if jsRes.resourceType == "binary" then
      UnwrapBinaryResource(jsRes.asInstanceOf[jsapi.BinaryResource])
    else
      UnwrapDirectoryResource(jsRes.asInstanceOf[jsapi.DirectoryResource[jsapi.BinaryResource]])

  final class UnwrapBinaryResource(jsRes: jsapi.BinaryResource) extends BinaryResource[R, E] {
    override def fileName: Option[String] =
      Nullable(jsRes.fileName).toOption

    override def asBytes: ZStream[R, E, Byte] =
      AsyncIterableTools.asyncIterableToZStream(jsRes.asAsyncIterable())
        .map(TypedArrayUtil.toByteChunk)
        .flattenChunks

  }

  final class UnwrapDirectoryResource(jsRes: jsapi.DirectoryResource[jsapi.BinaryResource]) extends DirectoryResource[R, E, BinaryResource] {
    override def fileName: Option[String] =
      Nullable(jsRes.fileName).toOption

    override def contents: ZStream[R, E, DirectoryEntry[R, E, BinaryResource]] =
      AsyncIterableTools.asyncIterableToZStream(jsRes.contents())
        .map { entry =>
          if entry.entryType == "subdirectory" then
            val entry2 = entry.asInstanceOf[jsapi.DirectoryEntry.Subdirectory[jsapi.BinaryResource]]
            DirectoryEntry.Subdirectory(entry2.name, UnwrapDirectoryResource(entry2.resource))
          else
            val entry2 = entry.asInstanceOf[jsapi.DirectoryEntry.File[jsapi.BinaryResource]]
            DirectoryEntry.File(entry2.name, UnwrapBinaryResource(entry2.resource))
          end if
        }
  }

  final class UnwrapResourceFactory(jsResFactory: jsapi.ResourceFactory) extends ResourceFactory[R, E] {
    override def directoryResource(name: String): DirectoryResource[R, E, BinaryResource] =
      UnwrapDirectoryResource(jsResFactory.directoryResource(name))

    override def binaryResource(name: String): BinaryResource[R, E] =
      UnwrapBinaryResource(jsResFactory.binaryResource(name))
  }

  object UnwrapResourceFactory {
    def apply(jsResFactory: jsapi.ResourceFactory): ResourceFactory[R, E] =
      jsResFactory match {
        case jsResFactory: WrapResourceFactory => jsResFactory.inner
        case _ => new UnwrapResourceFactory(jsResFactory)
      }
  }

  final class UnwrapTubeLoader[ContextOptions, JLibOptions]
  (jTubeLoader: jsapi.TubeLoader[JLibOptions])
    extends TubeLoader[R, E, ContextOptions] {

    override type LibOptions = JLibOptions
    override def libOptionDecoder(using contextOptionsDecoder: OptionDecoder[R, E, ContextOptions]): OptionDecoder[R, E, JLibOptions] =
      UnwrapOptionDecoder(jTubeLoader.libOptionsDecoder)

    override def load
    (ctx: Context { type Env = R; type Error = E; type Options = ContextOptions })
    (tubeImporter2: TubeImporter & HasContext[ctx.type])
    (libOptions: JLibOptions)
    : ZIO[R & Scope, E, ArTubeC & HasContext[ctx.type]] =
      JSPromiseUtil.promiseToEffect(jTubeLoader.load(WrapTubeImporter(ctx)(tubeImporter2), libOptions))
        .withFinalizer(tube => ZIO.fromPromiseJS(tube.close()).orDie)
        .flatMap { serTube =>
          new TubeReaderFactory {
            override val context: ctx.type = ctx
            override protected val serialized: SerializedTube[context.Env, context.Error] = UnwrapSerializedTube(serTube)
            override protected val tubeImporter: TubeImporter & HasContext[context.type] = tubeImporter2
          }.create
        }
        .flatMap(_.asTube)

  }

  final class UnwrapSerializedTube(jTube: jsapi.SerializedTube) extends SerializedTube[R, E] {

    private def unwrapProtoPromise[A <: GeneratedMessage, B <: js.Any](companion: GeneratedMessageCompanion[A], jsCodec: jsapi.proto.TSProtoCodec[B])(b: js.Promise[B]): ZIO[R, E, A] =
      JSPromiseUtil.promiseToEffect(b).map(convertProtoFromJS(companion, jsCodec))


    override def version: ZIO[R, E, t.TubeFormatVersion] =
      unwrapProtoPromise(t.TubeFormatVersion, jsapi.proto.TubeFormatVersion)(jTube.version())

    override def metadata: ZIO[R, E, t.Metadata] =
      unwrapProtoPromise(t.Metadata, jsapi.proto.Metadata)(jTube.metadata())

    override def getResource(id: String): ZIO[R, E, FileSystemResource[R, E]] =
      JSPromiseUtil.promiseToEffect(jTube.getResource(id)).map(unwrapFileSystemResource)

    override def getModule(modulePath: t.ModulePath): ZIO[R, E, t.ModuleDefinition] =
      val jsMoudlePath = convertProtoToJS(t.ModulePath, jsapi.proto.ModulePath)(modulePath)
      unwrapProtoPromise(t.ModuleDefinition, jsapi.proto.ModuleDefinition)(jTube.getModule(jsMoudlePath))

    override def getClass(id: BigInt): ZIO[R, E, t.ClassDefinition] =
      unwrapProtoPromise(t.ClassDefinition, jsapi.proto.ClassDefinition)(jTube.getClass(js.BigInt(id.toString())))

    override def getTrait(id: BigInt): ZIO[R, E, t.TraitDefinition] =
      unwrapProtoPromise(t.TraitDefinition, jsapi.proto.TraitDefinition)(jTube.getTrait(js.BigInt(id.toString())))

    override def getFunction(id: BigInt): ZIO[R, E, t.FunctionDefinition] =
      unwrapProtoPromise(t.FunctionDefinition, jsapi.proto.FunctionDefinition)(jTube.getFunction(js.BigInt(id.toString())))

    override def getMethod(id: BigInt): ZIO[R, E, t.MethodDefinition] =
      unwrapProtoPromise(t.MethodDefinition, jsapi.proto.MethodDefinition)(jTube.getMethod(js.BigInt(id.toString())))

    override def getClassConstructor(id: BigInt): ZIO[R, E, t.ClassConstructorDefinition] =
      unwrapProtoPromise(t.ClassConstructorDefinition, jsapi.proto.ClassConstructorDefinition)(jTube.getClassConstructor(js.BigInt(id.toString())))
  }

  object UnwrapSerializedTube {
    def apply(jTube: jsapi.SerializedTube): SerializedTube[R, E] =
      jTube match {
        case jTube: WrapSerializedTube => jTube.inner
        case _ => new UnwrapSerializedTube(jTube)
      }
  }

  final class UnwrapPlugin[JSOptions, JSOutput, JSExternMethod, JSExternFunction, JSExternClassConstructor]
  (jsPlugin: jsapi.Plugin[JSOptions, JSOutput, JSExternMethod, JSExternFunction, JSExternClassConstructor])
    extends Plugin[R, E] {

    override type Options = JSOptions
    override type Output = JSOutput

    override def optionCodec: OptionCodec[R, E, JSOptions] = UnwrapOptionCodec(jsPlugin.optionCodec)

    override def outputHandler: OutputHandler[R, E, Output] = UnwrapOutputHandler(jsPlugin.outputHandler)


    override type ExternMethodImplementation = JSExternMethod
    override type ExternFunctionImplementation = JSExternFunction
    override type ExternClassConstructorImplementation = JSExternClassConstructor


    override def emitTube
    (context: Context {type Env = R; type Error = E})
    (adapter: PluginContextAdapter.Aux[context.type, UnwrapPlugin.this.type])
    (tube: ArTubeC & HasContext[context.type] & HasImplementation[true]): context.Comp[JSOutput] =
      TubeSerializer.ofImplementation(context)(tube).flatMap { serializedTube =>
        JSPromiseUtil.promiseToEffect {
          jsPlugin.emitTube(WrapSerializedTube(serializedTube))
        }
      }

    override def loadExternMethod(options: JSOptions)(id: String): ZIO[R, E, Option[JSExternMethod]] =
      JSPromiseUtil.promiseToEffect {
        jsPlugin.loadExternMethod(options, id)
      }.map { extern => Nullable(extern).toOption }

    override def loadExternFunction(options: JSOptions)(id: String): ZIO[R, E, Option[JSExternFunction]] =
      JSPromiseUtil.promiseToEffect {
        jsPlugin.loadExternFunction(options, id)
      }.map { extern => Nullable(extern).toOption }

    override def loadExternClassConstructor(options: JSOptions)(id: String): ZIO[R, E, Option[JSExternClassConstructor]] =
      JSPromiseUtil.promiseToEffect {
        jsPlugin.loadExternClassConstructor(options, id)
      }.map { extern => Nullable(extern).toOption }

    override def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, ContextOptions]] =
      jsPlugin.tubeLoaders
        .view
        .mapValues { factory =>
          factory.withLoader(new js.Object with jsapi.LoaderConsumer[TubeLoader[R, E, ContextOptions]] {
            override def consume[LibOptions](loader: jsapi.TubeLoader[LibOptions]): TubeLoader[R, E, ContextOptions] =
              UnwrapTubeLoader(loader)
          })
        }
        .toMap
  }
}
