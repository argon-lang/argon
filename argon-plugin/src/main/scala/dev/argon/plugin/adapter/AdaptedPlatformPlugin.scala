package dev.argon.plugin.adapter

import dev.argon.compiler.DefinitionInfo
import dev.argon.plugin.*
import dev.argon.options.OptionDecoder
import dev.argon.plugin.scalaApi
import dev.argon.tube
import zio.*
import zio.interop.catz.core.given
import cats.data.OptionT
import dev.argon.util.async.ErrorWrapper
import dev.argon.options.OutputHandler
import dev.argon.compiler.{ArTubeC, HasContext}

final class AdaptedPlatformPlugin[Externs, E >: PluginError, EX <: Throwable, PO] private(
  override val pluginId: String,
  plugin: scalaApi.PlatformPlugin[Externs, EX, PO],
  pluginEmitter: Option[scalaApi.TubeEmitter[Externs, EX, ?, ?]],
)(using errorWrapper: ErrorWrapper.Aux[E, EX]) extends PlatformPlugin[E] {
  type PlatformOptions = PO

  override def optionDecoder: OptionDecoder[PO] =
    AdaptedOptionDecoder(plugin.optionDecoder())


  final case class FunctionReferenceWrapper(inner: scalaApi.ExternFunctionRef[Externs]) extends ExternWrapper[scalaApi.ExternFunctionRef[Externs]]
  final case class FunctionImplementationWrapper(inner: scalaApi.ExternFunctionImpl[Externs]) extends ExternWrapper[scalaApi.ExternFunctionImpl[Externs]]


  final case class RecordReferenceWrapper(inner: scalaApi.ExternRecordRef[Externs]) extends ExternWrapper[scalaApi.ExternRecordRef[Externs]]


  trait ExternFunctionInfo extends Extern.Tagged {
    override type Reference = FunctionReferenceWrapper
    override type Implementation = FunctionImplementationWrapper
  }

  override val externFunction: ExternFunctionInfo = new ExternFunctionInfo {
    
    override def referenceTag: Tag[Reference] = summon[Tag[Reference]]
    
    override def implementationTag: Tag[Implementation] = summon[Tag[Implementation]]


    override def referenceCodec: ESExprCodecAsync[Reference] =
      AdaptedESExprCodecAsync(plugin.functionReferenceCodec())(FunctionReferenceWrapper.apply)


    override def implementationCodec: ESExprCodecAsync[Implementation] =
      AdaptedESExprCodecAsync(plugin.functionImplementationCodec())(FunctionImplementationWrapper.apply)


    override def defineReference(options: PlatformOptions)(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, Reference] =
      val specifier = FormatAdapters.createImportSpecifier(definitionInfo)
      ErrorWrapper.unwrapEffect(plugin.defineFunctionReference(options, specifier))
        .map(FunctionReferenceWrapper.apply)
    end defineReference

    def loadExtern(options: PlatformOptions)(id: String): cats.data.OptionT[[A] =>> zio.ZIO[PluginEnv, E, A], Implementation] =
      OptionT(ErrorWrapper.unwrapEffect(plugin.loadExternFunction(options, id)))
        .map(FunctionImplementationWrapper.apply)
  }


  trait ExternRecordInfo extends Extern.TaggedRef {
    override type Reference = RecordReferenceWrapper
  }

  override val externRecord: ExternRecordInfo = new ExternRecordInfo {

    override def referenceTag: Tag[Reference] = summon[Tag[Reference]]


    override def referenceCodec: ESExprCodecAsync[Reference] =
      AdaptedESExprCodecAsync(plugin.recordReferenceCodec())(RecordReferenceWrapper.apply)


    override def defineReference(options: PlatformOptions)(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, Reference] =
      val specifier = FormatAdapters.createImportSpecifier(definitionInfo)
      ErrorWrapper.unwrapEffect(plugin.defineRecordReference(options, specifier))
        .map(RecordReferenceWrapper.apply)
    end defineReference
  }

  override def emitter[Ctx <: ContextIncluding]: Option[TubeEmitter[E, Ctx]] =
    pluginEmitter.map { case tubeEmitter: scalaApi.TubeEmitter[Externs, EX, outOpts, out] =>
      new TubeEmitter[E, Ctx] {
        override type OutputOptions = outOpts
        override type Output = out

        override def outputOptionsDecoder: OptionDecoder[OutputOptions] =
          AdaptedOptionDecoder(tubeEmitter.outputOptionsDecoder())

        override def outputHandler: OutputHandler[E, Output] =
          AdaptedOutputHandler(tubeEmitter.outputHandler())

        override def emitTube(context: Ctx)(tube: ArTubeC & HasContext[context.type])(options: OutputOptions): context.Comp[Output] =
          VMAdapter(AdaptedPlatformPlugin.this)(context)
            .flatMap { vmAdapter => vmAdapter.getTube(tube) }
            .flatMap { tube2 => ErrorWrapper.unwrapEffect(tubeEmitter.emitTube(tube2, options)) }
      }
    }

  override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[String, TubeLoader[Ctx]] = ???

}

object AdaptedPlatformPlugin {
  // def apply[Externs, E >: PluginError, EX <: Throwable](): UIO[AdaptedPlatformPlugin[Externs, E >: PluginError, EX <: Throwable, PO]] =
  //   for
  //     override val pluginId: String,
  // plugin: scalaApi.PlatformPlugin[Externs, EX, PO],
  // pluginEmitter: Option[scalaApi.TubeEmitter[Externs, EX, ?, ?]],

  def makeFactory[Externs, PO](makePlugin: [EX <: Throwable] => () => scalaApi.PlatformPlugin[Externs, EX, PO]): PluginFactory =
    new PluginFactory.OfPlatform {
      override def create[E >: PluginError]: UIO[PlatformPlugin[E]] =
        ZIO.succeed {
          ErrorWrapperContainer[E]
        }
        .flatMap { ewc =>
          given ew: ErrorWrapper[E] = ErrorWrapper.forWrappedError[E, ewc.WrappedError](ewc.WrappedError(_))
          val plugin = makePlugin[ew.EX]()

          for
            id <- plugin.pluginId()
            emitterFactory <- plugin.emitter()
            emitter <- ZIO.foreach(emitterFactory) { emitterFactory =>
              emitterFactory.create(
                new scalaApi.TubeEmitterFactoryCallback[Externs, ew.EX, scalaApi.TubeEmitter[Externs, ew.EX, ?, ?]] {
                  def call[OutputOptions, Output](emitter: scalaApi.TubeEmitter[Externs, ew.EX, OutputOptions, Output]): UIO[scalaApi.TubeEmitter[Externs, ew.EX, ?, ?]] =
                    ZIO.succeed(emitter)
                }
              )
            }
          yield AdaptedPlatformPlugin(
              id,
              plugin,
              emitter,
            )          
        }
    }
    
  private final class ErrorWrapperContainer[E] {
    final class WrappedError(cause: Cause[E]) extends ErrorWrapper.WrappedErrorBase[E](cause)
  }

}

