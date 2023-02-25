package dev.argon.plugin.loader.java

import dev.argon.compiler.{Context, HasContext}
import dev.argon.compiler.tube.{ArTubeC, TubeImporter, TubeName}
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.io.*
import dev.argon.options.{OptionCodec, OptionDecoder, OutputHandler, OutputInfo}
import zio.*
import zio.stream.*
import dev.argon.plugin.*
import dev.argon.plugin.api as japi
import dev.argon.plugin.api.options
import dev.argon.plugin.api.options.OptionDecodeException
import dev.argon.plugin.tube.{InvalidTube, SerializedTube, TubeReaderBase, TubeReaderFactory, TubeSerializer}
import dev.argon.tube as t
import dev.argon.util.{ErrorWrapper, JavaExecuteIO, NonEmptyList}
import dev.argon.util.toml.Toml

import java.io.{FilterInputStream, IOException, InputStream}
import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.channels.SeekableByteChannel
import java.util.{Optional, Spliterator, Spliterators}
import java.util.stream.{StreamSupport, Stream as JStream}
import scala.reflect.TypeTest
import scala.jdk.OptionConverters.*
import scala.jdk.CollectionConverters.*

final class WrapperContext[R, E >: InvalidTube](using runtime: Runtime[R]) {
  final class WrappedError(private[WrapperContext] val value: Cause[E]) extends IOException
  final class WrappedOptionDecodeException(message: String, private[WrapperContext] val value: Cause[String]) extends OptionDecodeException(message)

  private given ErrorWrapper[E, WrappedError] with
    override def wrap(error: Cause[E]): WrappedError = WrappedError(error)
    override def unwrap(ex: WrappedError): Cause[E] = ex.value
  end given

  private given ErrorWrapper[String, OptionDecodeException] with
    override def wrap(error: Cause[String]): OptionDecodeException =
      WrappedOptionDecodeException(
        error.failureOption.getOrElse("Unknown error"),
        error
      )

    override def unwrap(ex: OptionDecodeException): Cause[String] =
      ex match {
        case ex: WrappedOptionDecodeException => ex.value
        case _ => Cause.fail(ex.getMessage)
      }
  end given


  private def refineError[EX2 >: WrappedError <: Throwable](ex: EX2): IO[E, Nothing] =
    ex match {
      case ex: WrappedError => ZIO.failCause(ex.value)
      case _ => ZIO.die(ex)
    }

  private def refineStream[EX2 >: WrappedError <: Throwable](ex: EX2): Stream[E, Nothing] =
    ZStream.fromZIO(refineError(ex))

  private def wrapErrorCause(cause: Cause[E]): Cause[WrappedError] =
    Cause.fail(WrappedError(cause))

  private def unwrapError(ex: WrappedError): Cause[E] =
    ex.value

  final class Operations extends japi.PluginOperations[WrappedError]


  def wrapEffect[A](a: ZIO[R, E, A]): A =
    JavaExecuteIO.runInterruptable(a)


  final class WrapResourceFactory(val inner: ResourceFactory[R, E]) extends japi.ResourceFactory[WrappedError] {
    override def directoryResource(name: String): japi.DirectoryResource[WrappedError, api.BinaryResource[WrappedError]] =
      WrapDirectoryResource(inner.directoryResource(name))

    override def binaryResource(name: String): japi.BinaryResource[WrappedError] =
      WrapBinaryResource(inner.binaryResource(name))
  }

  final class WrapResourceRecorder(val inner: ResourceRecorder[R, E]) extends japi.ResourceRecorder[WrappedError] {
    override def recordBinaryResource(resource: japi.BinaryResource[WrappedError]): String =
      wrapEffect(inner.recordBinaryResource(UnwrapBinaryResource(resource)))

    override def recordDirectoryResource(resource: japi.DirectoryResource[WrappedError, api.BinaryResource[WrappedError]]): String =
      wrapEffect(inner.recordDirectoryResource(UnwrapDirectoryResource(resource)))
  }

  final class WrapInputStream(is: InputStream, scope: Scope.Closeable) extends FilterInputStream(is) {
    override def close(): Unit =
      try super.close()
      finally wrapEffect(scope.close(Exit.unit))
  }

  final class WrapSeekableByteChannel(channel: SeekableByteChannel, scope: Scope.Closeable) extends SeekableByteChannel {
    override def read(dst: ByteBuffer | Null): RuntimeFlags =
      channel.read(dst)

    override def write(src: ByteBuffer | Null): RuntimeFlags =
      channel.write(src)

    override def position(): Long =
      channel.position()

    override def position(newPosition: Long): SeekableByteChannel | Null =
      channel.position(newPosition)

    override def size(): Long =
      channel.size()

    override def truncate(size: Long): SeekableByteChannel | Null =
      channel.truncate(size)

    override def isOpen: Boolean =
      channel.isOpen

    override def close(): Unit =
      try channel.close()
      finally wrapEffect(scope.close(Exit.unit))
  }

  final class WrapBinaryResource(res: BinaryResource[R, E]) extends japi.BinaryResource[WrappedError] {
    override def asInputStream(): InputStream =
      wrapEffect(
        for
          scope <- Scope.make
          is <- scope.extend(
            res.asInputStream
              .getOrElse(
                res.asBytes
                  .mapErrorCause(wrapErrorCause)
                  .toInputStream
                  .catchAll(ex => ZIO.failCause[E](unwrapError(ex)))
              )
          )
        yield WrapInputStream(is, scope)
      )

    override def asSeekableByteChannel(): Optional[japi.SupplierWithError[WrappedError, SeekableByteChannel]] =
      res.asSeekableByteChannel.map[japi.SupplierWithError[WrappedError, SeekableByteChannel]](channelEffect => () => wrapEffect(
        for
          scope <- Scope.make
          channel <- scope.extend(channelEffect)
        yield WrapSeekableByteChannel(channel, scope)
      )).toJava

    override def fileName(): Optional[String] = res.fileName.toJava
  }

  final class WrapDirectoryResource(res: DirectoryResource[R, E, BinaryResource]) extends japi.DirectoryResource[WrappedError, japi.BinaryResource[WrappedError]] {
    override def contents(): JStream[api.DirectoryEntry[WrappedError, japi.BinaryResource[WrappedError]]] =
      wrapEffect(
        for
          scope <- Scope.make
          iter <- scope.extend(
            res.contents
              .map[japi.DirectoryEntry[WrappedError, japi.BinaryResource[WrappedError]]] {
                case DirectoryEntry.Subdirectory(name, resource) => new japi.DirectoryEntry.Subdirectory(name, WrapDirectoryResource(resource))
                case DirectoryEntry.File(name, resource) => new japi.DirectoryEntry.File(name, WrapBinaryResource(resource))
              }
              .mapErrorCause(wrapErrorCause)
              .toIterator
          )

          iterJ = iter
            .map {
              case Left(ex) => throw ex
              case Right(entry) => entry
            }
            .asJava

        yield StreamSupport.stream(Spliterators.spliteratorUnknownSize(iterJ, Spliterator.ORDERED), false).nn
          .onClose(() => wrapEffect(scope.close(Exit.unit))).nn
      )

    override def fileName(): Optional[String] = res.fileName.toJava
  }

  def wrapFileSystemResource(res: FileSystemResource[R, E]): japi.FileSystemResource[WrappedError] =
    res match {
      case res: BinaryResource[R, E] => WrapBinaryResource(res)
      case res: DirectoryResource[R, E, BinaryResource] => WrapDirectoryResource(res)
    }

  final class WrapSerializedTube(val inner: SerializedTube[R, E]) extends japi.tube.SerializedTube[WrappedError] {
    override def version(): japi.tube.TubeFormatVersion =
      wrapEffect(inner.version.map(t.TubeFormatVersion.toJavaProto))

    override def metadata(): japi.tube.Metadata =
      wrapEffect(inner.metadata.map(t.Metadata.toJavaProto))

    override def getResource(id: String): japi.FileSystemResource[WrappedError] =
      wrapEffect(inner.getResource(id).map(wrapFileSystemResource))

    override def getModule(modulePath: japi.tube.ModulePath): japi.tube.ModuleDefinition =
      wrapEffect(inner.getModule(t.ModulePath.fromJavaProto(modulePath)).map(t.ModuleDefinition.toJavaProto))

    override def getClass(id: BigInteger): japi.tube.ClassDefinition =
      wrapEffect(inner.getClass(id).map(t.ClassDefinition.toJavaProto))

    override def getTrait(id: BigInteger): japi.tube.TraitDefinition =
      wrapEffect(inner.getTrait(id).map(t.TraitDefinition.toJavaProto))

    override def getFunction(id: BigInteger): japi.tube.FunctionDefinition =
      wrapEffect(inner.getFunction(id).map(t.FunctionDefinition.toJavaProto))

    override def getMethod(id: BigInteger): japi.tube.MethodDefinition =
      wrapEffect(inner.getMethod(id).map(t.MethodDefinition.toJavaProto))

    override def getClassConstructor(id: BigInteger): japi.tube.ClassConstructorDefinition =
      wrapEffect(inner.getClassConstructor(id).map(t.ClassConstructorDefinition.toJavaProto))

    override def close(): Unit = ()
  }

  private final class WrapTubeImporter(context: Context { type Env = R; type Error = E })(inner: TubeImporter & HasContext[context.type]) extends japi.TubeImporter[WrappedError] {
    override def getTube(tubeName: japi.tube.TubeName): japi.tube.SerializedTube[WrappedError] =
      wrapEffect(
        inner.getTube(TubeName(NonEmptyList.fromList(tubeName.getNameList.nn.asScala.toList).get))
          .flatMap(TubeSerializer.ofInterface(context)(_))
          .map(WrapSerializedTube(_))
      )
  }



  def unwrapEffect[A](a: => A): IO[E, A] =
    ZIO.attemptBlocking { a }
      .catchAll(refineError)


  class UnwrapOptionDecoder[A](jOptionDecoder: options.OptionDecoder[WrappedError, A]) extends OptionDecoder[R, E, A] {
    override final def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A] =
      val value2 = t.Toml.toJavaProto(t.TomlConverter.encodeToml(value))
      try Right(jOptionDecoder.decode(WrapResourceFactory(resFactory), value2))
      catch {
        case ex: OptionDecodeException => Left(ex.getMessage)
      }
    end decode

    override final def defaultValue: Option[A] = jOptionDecoder.defaultValue().toScala
  }

  object UnwrapOptionDecoder {
    def apply[A](jOptionDecoder: options.OptionDecoder[WrappedError, A]): OptionDecoder[R, E, A] =
      new UnwrapOptionDecoder[A](jOptionDecoder)
  }

  final class UnwrapOptionCodec[A](jOptionCodec: options.OptionCodec[WrappedError, A]) extends UnwrapOptionDecoder[A](jOptionCodec) with OptionCodec[R, E, A] {
    override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Toml] =
      unwrapEffect {
        jOptionCodec.encode(WrapResourceRecorder(recorder), value)
      }
        .map { toml =>
          t.TomlConverter.decodeToml(t.Toml.fromJavaProto(toml))
        }
  }

  final class UnwrapOutputHandler[A](jOptionCodec: options.OutputHandler[WrappedError, A]) extends OutputHandler[R, E, A] {
    override lazy val options: Map[Seq[String], OutputInfo[R, E, A]] =
      jOptionCodec.options()
        .asScala
        .view
        .map {
          case (optPath, outputInfo) =>
            (optPath.asScala.toSeq, UnwrapOutputInfo(outputInfo))
        }
        .toMap
  }

  final class UnwrapOutputInfo[A](jOptionCodec: options.OutputInfo[WrappedError, A]) extends OutputInfo[R, E, A] {
    override def getValue(options: A): FileSystemResource[R, E] =
      unwrapFileSystemResource(jOptionCodec.getValue(options))
  }


  def unwrapFileSystemResource(jRes: japi.FileSystemResource[WrappedError]): FileSystemResource[R, E] =
    jRes match
      case resource: japi.BinaryResource[WrappedError] => UnwrapBinaryResource(resource)
      case resource: japi.DirectoryResource[WrappedError, ?] => UnwrapDirectoryResource(resource)
      case _ => throw MatchError(jRes)
    end match

  final class UnwrapBinaryResource(jRes: japi.BinaryResource[WrappedError]) extends BinaryResource[R, E] {
    override def fileName: Option[String] =
      jRes.fileName().toScala

    override def asBytes: ZStream[R, E, Byte] =
      ZStream.unwrapScoped(
        ZIO.fromAutoCloseable(
          unwrapEffect {
            jRes.asInputStream()
          }
        )
          .map { is =>
            ZStream.fromInputStream(is)
              .catchAll(refineStream)
          }
      )


    override def asInputStream: Option[ZIO[R & Scope, E, InputStream]] =
      Some(ZIO.fromAutoCloseable(unwrapEffect { jRes.asInputStream() }))

    override def asSeekableByteChannel: Option[ZIO[R & Scope, E, SeekableByteChannel]] =
      jRes.asSeekableByteChannel()
        .toScala
        .map { supplier => unwrapEffect { supplier.get() } }

  }

  final class UnwrapDirectoryResource(jRes: japi.DirectoryResource[WrappedError, ?]) extends DirectoryResource[R, E, BinaryResource] {
    override def fileName: Option[String] =
      jRes.fileName().toScala

    override def contents: ZStream[R, E, DirectoryEntry[R, E, BinaryResource]] =
      ZStream.fromJavaStream {
        jRes.contents()
      }
        .catchAll(refineStream)
        .map {
          case subdir: japi.DirectoryEntry.Subdirectory[WrappedError, ?] =>
            DirectoryEntry.Subdirectory(subdir.name(), UnwrapDirectoryResource(subdir.resource()))

          case file: japi.DirectoryEntry.File[WrappedError, ?] =>
            DirectoryEntry.File(file.name(), UnwrapBinaryResource(file.resource()))

          case entry => throw MatchError(entry)
        }
  }

  final class UnwrapResourceFactory(jResFactory: japi.ResourceFactory[WrappedError]) extends ResourceFactory[R, E] {
    override def directoryResource(name: String): DirectoryResource[R, E, BinaryResource] =
      UnwrapDirectoryResource(jResFactory.directoryResource(name))

    override def binaryResource(name: String): BinaryResource[R, E] =
      UnwrapBinaryResource(jResFactory.binaryResource(name))
  }

  object UnwrapResourceFactory {
    def apply(jResFactory: japi.ResourceFactory[WrappedError]): ResourceFactory[R, E] =
      jResFactory match {
        case jResFactory: WrapResourceFactory => jResFactory.inner
        case _ => new UnwrapResourceFactory(jResFactory)
      }
  }

  final class UnwrapTubeLoader[ContextOptions, JLibOptions]
  (jTubeLoader: japi.TubeLoader[WrappedError, JLibOptions])
    extends TubeLoader[R, E, ContextOptions] {

    override type LibOptions = JLibOptions
    override def libOptionDecoder(using contextOptionsDecoder: OptionDecoder[R, E, ContextOptions]): OptionDecoder[R, E, JLibOptions] =
      UnwrapOptionDecoder(jTubeLoader.libOptionsDecoder())

    override def load
    (ctx: Context { type Env = R; type Error = E; type Options = ContextOptions })
    (tubeImporter2: TubeImporter & HasContext[ctx.type])
    (libOptions: JLibOptions)
    : ZIO[R & Scope, E, ArTubeC & HasContext[ctx.type]] =
      unwrapEffect(jTubeLoader.load(WrapTubeImporter(ctx)(tubeImporter2), libOptions))
        .withFinalizerAuto
        .flatMap { serTube =>
          new TubeReaderFactory {
            override val context: ctx.type = ctx
            override protected val serialized: SerializedTube[context.Env, context.Error] = UnwrapSerializedTube(serTube)
            override protected val tubeImporter: TubeImporter & HasContext[context.type] = tubeImporter2
          }.create
        }
        .flatMap(_.asTube)

  }

  final class UnwrapSerializedTube(jTube: japi.tube.SerializedTube[WrappedError]) extends SerializedTube[R, E] {
    override def version: ZIO[R, E, t.TubeFormatVersion] =
      unwrapEffect(jTube.version()).map(t.TubeFormatVersion.fromJavaProto)

    override def metadata: ZIO[R, E, t.Metadata] =
      unwrapEffect(jTube.metadata()).map(t.Metadata.fromJavaProto)

    override def getResource(id: String): ZIO[R, E, FileSystemResource[R, E]] =
      unwrapEffect(jTube.getResource(id)).map(unwrapFileSystemResource)

    override def getModule(modulePath: t.ModulePath): ZIO[R, E, t.ModuleDefinition] =
      unwrapEffect(jTube.getModule(t.ModulePath.toJavaProto(modulePath))).map(t.ModuleDefinition.fromJavaProto)

    override def getClass(id: BigInt): ZIO[R, E, t.ClassDefinition] =
      unwrapEffect(jTube.getClass(id.bigInteger)).map(t.ClassDefinition.fromJavaProto)

    override def getTrait(id: BigInt): ZIO[R, E, t.TraitDefinition] =
      unwrapEffect(jTube.getTrait(id.bigInteger)).map(t.TraitDefinition.fromJavaProto)

    override def getFunction(id: BigInt): ZIO[R, E, t.FunctionDefinition] =
      unwrapEffect(jTube.getFunction(id.bigInteger)).map(t.FunctionDefinition.fromJavaProto)

    override def getMethod(id: BigInt): ZIO[R, E, t.MethodDefinition] =
      unwrapEffect(jTube.getMethod(id.bigInteger)).map(t.MethodDefinition.fromJavaProto)

    override def getClassConstructor(id: BigInt): ZIO[R, E, t.ClassConstructorDefinition] =
      unwrapEffect(jTube.getClassConstructor(id.bigInteger)).map(t.ClassConstructorDefinition.fromJavaProto)
  }

  object UnwrapSerializedTube {
    def apply(jTube: japi.tube.SerializedTube[WrappedError]): SerializedTube[R, E] =
      jTube match {
        case jTube: WrapSerializedTube => jTube.inner
        case _ => new UnwrapSerializedTube(jTube)
      }
  }

  final class UnwrapPlugin[JOptions, JOutput, JExternMethod, JExternFunction, JExternClassConstructor]
  (jPlugin: japi.Plugin[WrappedError, JOptions, JOutput, JExternMethod, JExternFunction, JExternClassConstructor])
    extends Plugin[R, E] {

    override type Options = JOptions
    override type Output = JOutput

    override def optionCodec: OptionCodec[R, E, JOptions] = UnwrapOptionCodec(jPlugin.optionCodec())

    override def outputHandler: OutputHandler[R, E, Output] = UnwrapOutputHandler(jPlugin.outputHandler())


    override type ExternMethodImplementation = JExternMethod
    override type ExternFunctionImplementation = JExternFunction
    override type ExternClassConstructorImplementation = JExternClassConstructor


    override def emitTube
    (context: Context {type Env = R; type Error = E})
    (adapter: PluginContextAdapter.Aux[context.type, UnwrapPlugin.this.type])
    (tube: ArTubeC & HasContext[context.type] & HasImplementation[true]): context.Comp[JOutput] =
      TubeSerializer.ofImplementation(context)(tube).flatMap { serializedTube =>
        unwrapEffect {
          jPlugin.emitTube(WrapSerializedTube(serializedTube))
        }
      }

    override def loadExternMethod(options: JOptions)(id: String): ZIO[R, E, Option[JExternMethod]] =
      unwrapEffect {
        jPlugin.loadExternMethod(options, id).toScala
      }

    override def loadExternFunction(options: JOptions)(id: String): ZIO[R, E, Option[JExternFunction]] =
      unwrapEffect {
        jPlugin.loadExternFunction(options, id).toScala
      }

    override def loadExternClassConstructor(options: JOptions)(id: String): ZIO[R, E, Option[JExternClassConstructor]] =
      unwrapEffect {
        jPlugin.loadExternClassConstructor(options, id).toScala
      }

    override def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, ContextOptions]] =
      jPlugin.tubeLoaders()
        .asScala
        .view
        .mapValues(UnwrapTubeLoader(_))
        .toMap
  }
}
