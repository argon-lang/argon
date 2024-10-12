package dev.argon.plugin.adapter

import dev.argon.compiler.DefinitionInfo
import dev.argon.plugin.*
import dev.argon.options.OptionDecoder
import dev.argon.plugin.scalaApi
import dev.argon.tube
import zio.*
import zio.interop.catz.core.given
import cats.data.OptionT

class AdaptedPlatformPlugin[Externs, E >: PluginError, PO](
  override val pluginId: String,
  plugin: scalaApi.PlatformPlugin[Externs, E, PO]
) extends PlatformPlugin[E] {
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
      plugin.defineFunctionReference(options, specifier)
        .map(FunctionReferenceWrapper.apply)
    end defineReference

    def loadExtern(options: PlatformOptions)(id: String): cats.data.OptionT[[A] =>> zio.ZIO[PluginEnv, E, A], Implementation] =
      OptionT(plugin.loadExternFunction(options, id))
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
      plugin.defineRecordReference(options, specifier)
        .map(RecordReferenceWrapper.apply)
    end defineReference
  }

  override def emitter[Ctx <: ContextIncluding]: Option[TubeEmitter[Ctx]] = ???

  override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[String, TubeLoader[Ctx]] = ???

  
}
