package dev.argon.plugin

import dev.argon.util.given
import cats.data.OptionT
import dev.argon.compiler.*
import esexpr.ESExprCodec
import dev.argon.options.{OptionDecoder, OutputHandler}
import dev.argon.plugin.PlatformPluginSet.Empty
import dev.argon.plugin.PluginSetUtil.*
import zio.*
import zio.interop.catz.core.given

abstract class PlatformPlugin[E >: PluginError] extends ExternContext[E] {
  val pluginId: String

  type PlatformOptions

  given optionDecoder: OptionDecoder[PlatformOptions]


  val externFunction: Extern.Tagged
  val externRecord: Extern.TaggedRef


  type ContextIncluding = Context {
    type Env <: PluginEnv
    type Error = E
    val implementations: {
      type ExternFunctionImplementation <: ZEnvironment[externFunction.Implementation]
      type FunctionReference <: ZEnvironment[externFunction.Reference]
      type RecordReference <: ZEnvironment[externRecord.Reference]
    }
  }

  type ContextOnlyIncluding = Context {
    type Env <: PluginEnv
    type Error = E
    val implementations: {
      type ExternFunctionImplementation = ZEnvironment[externFunction.Implementation]
      type FunctionReference = ZEnvironment[externFunction.Reference]
      type RecordReference = ZEnvironment[externRecord.Reference]
    }
  }

  def emitter[Ctx <: ContextIncluding]: Option[TubeEmitter[E, Ctx]]
  def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[String, TubeLoader[Ctx]]

}

sealed trait PlatformPluginSet[E >: PluginError] {
  val pluginIds: Set[String]

  type PlatformOptions

  given optionDecoder: PluginSetUtil.PartialOptionDecoder[PlatformOptions]



  type ContextIncluding = Context {
    type Env <: PluginEnv
    type Error = E
    val implementations: {
      type ExternFunctionImplementation <: ZEnvironment[externFunction.Implementation]
      type FunctionReference <: ZEnvironment[externFunction.Reference]
      type RecordReference <: ZEnvironment[externRecord.Reference]
    }
  }

  type ContextOnlyIncluding = Context {
    type Env <: PluginEnv
    type Error = E
    val implementations: {
      type ExternFunctionImplementation = ZEnvironment[externFunction.Implementation]
      type FunctionReference = ZEnvironment[externFunction.Reference]
      type RecordReference = ZEnvironment[externRecord.Reference]
    }
  }

  val externFunction: PartialExtern
  val externRecord: PartialExternRef


  def emitter[Ctx <: ContextIncluding]: TubeEmitterSet[E, Ctx]
  def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[TubeLoaderName, TubeLoader[Ctx]]


  sealed trait PartialExternRef {
    type Reference
    given referenceCodec: PartialESExprCodec[Reference]
    given referenceEnvTag: EnvironmentTag[Reference]

    def defineReference
    (options: PlatformOptions)
    (definitionInfo: DefinitionInfo)
    : ZIO[PluginEnv, E, ZEnvironment[Reference]]


    def asExtern(externContext: ExternContext[E] { type PlatformOptions = PlatformPluginSet.this.PlatformOptions }): externContext.ExternRef {
      type Reference = ZEnvironment[PartialExternRef.this.Reference]
    } =
      new externContext.ExternRef {
        override type Reference = ZEnvironment[PartialExternRef.this.Reference]

        override def referenceCodec: ESExprCodecAsync[Reference] =
          PartialExternRef.this.referenceCodec.toCodec

        override def defineReference(options: externContext.PlatformOptions)(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, Reference] =
          PartialExternRef.this.defineReference(options)(definitionInfo)
      }

  }

  sealed trait PartialExtern extends PartialExternRef {
    type Implementation
    given implementationCodec: PartialESExprCodec[Implementation]
    given implementationEnvTag: EnvironmentTag[Implementation]

    def loadExtern
    (options: PlatformOptions)
    (id: String)
    : OptionT[[A] =>> ZIO[PluginEnv, E, A], ZEnvironment[Implementation]]


    override def asExtern(externContext: ExternContext[E] { type PlatformOptions = PlatformPluginSet.this.PlatformOptions }): externContext.Extern {
      type Implementation = ZEnvironment[PartialExtern.this.Implementation]
      type Reference = ZEnvironment[PartialExtern.this.Reference]
    } =
      new externContext.Extern {
        override type Implementation = ZEnvironment[PartialExtern.this.Implementation]

        override def implementationCodec: ESExprCodecAsync[Implementation] =
          PartialExtern.this.implementationCodec.toCodec

        override def loadExtern(options: externContext.PlatformOptions)(id: String): OptionT[[A] =>> ZIO[PluginEnv, E, A], Implementation] =
          PartialExtern.this.loadExtern(options)(id)

        override type Reference = ZEnvironment[PartialExtern.this.Reference]

        override def referenceCodec: ESExprCodecAsync[Reference] =
          PartialExtern.this.referenceCodec.toCodec

        override def defineReference(options: externContext.PlatformOptions)(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, Reference] =
          PartialExtern.this.defineReference(options)(definitionInfo)
      }
  }

}

private[plugin] object PlatformPluginSet {
  final class Empty[E >: PluginError] extends PlatformPluginSet[E] {
    override val pluginIds: Set[String] = Set.empty

    override type PlatformOptions = Unit

    override def optionDecoder: PluginSetUtil.PartialOptionDecoder[Unit] =
      PluginSetUtil.PartialOptionDecoderEmpty()


    override val externFunction: PartialExternEmpty =
      PartialExternEmpty()

    override val externRecord: PartialExternEmpty =
      PartialExternEmpty()

    override def emitter[Ctx <: ContextIncluding]: TubeEmitterSet[E, Ctx] =
      TubeEmitterSet.Empty()

    override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[TubeLoaderName, TubeLoader[Ctx]] = Map.empty

    final class PartialExternEmpty extends PartialExtern {
      override type Implementation = Any
      override def implementationCodec: PartialESExprCodec[Any] = PartialESExprCodecEmpty()
      override def implementationEnvTag: EnvironmentTag[Any] = summon

      override def loadExtern(options: PlatformOptions)(id: String): OptionT[[A] =>> ZIO[PluginEnv, E, A], ZEnvironment[Implementation]] =
        OptionT.some(ZEnvironment.empty)

      override type Reference = Any
      override def referenceCodec: PartialESExprCodec[Any] = PartialESExprCodecEmpty()
      override def referenceEnvTag: EnvironmentTag[Any] = summon

      override def defineReference(options: PlatformOptions)(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, ZEnvironment[Any]] =
        ZIO.succeed(ZEnvironment.empty)
    }
  }

  final class Singleton[E >: PluginError](val plugin: PlatformPlugin[E]) extends PlatformPluginSet[E] {
    override val pluginIds: Set[String] = Set(plugin.pluginId)

    override type PlatformOptions = plugin.PlatformOptions

    override def optionDecoder: PluginSetUtil.PartialOptionDecoder[PlatformOptions] =
      PluginSetUtil.PartialOptionDecoderSingleton[PlatformOptions](plugin.pluginId)


    override val externFunction: PartialExternSingleton[plugin.externFunction.type] =
      PartialExternSingleton(plugin.externFunction)

    override val externRecord: PartialExternRefSingleton[plugin.externRecord.type] =
      PartialExternRefSingleton(plugin.externRecord)

    override def emitter[Ctx <: ContextIncluding]: TubeEmitterSet[E, Ctx] =
      plugin.emitter[Ctx] match {
        case Some(emitter) => TubeEmitterSet.Singleton(emitter, plugin.pluginId)
        case None => TubeEmitterSet.Empty()
      }


    override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[TubeLoaderName, TubeLoader[Ctx]] =
      plugin.tubeLoaders[Ctx].map { (k, v) => TubeLoaderName(plugin.pluginId, k) -> v }


    
    sealed trait PartialExternRefSingleton[ET <: plugin.Extern.TaggedRef] extends PartialExternRef {
      val extern: ET
      import extern.given

      override type Reference = extern.Reference

      override def referenceCodec: PartialESExprCodec[Reference] =
        PartialESExprCodecSingleton[Reference](plugin.pluginId)

      override def referenceEnvTag: EnvironmentTag[Reference] = summon[EnvironmentTag[Reference]]

      override def defineReference(options: PlatformOptions)(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, ZEnvironment[extern.Reference]] =
        extern.defineReference(options)(definitionInfo).map(ZEnvironment.apply)
    }

    object PartialExternRefSingleton {
      def apply(extern2: plugin.Extern.TaggedRef): PartialExternRefSingleton[extern2.type] =
        new PartialExternRefSingleton[extern2.type] {
          override val extern: extern2.type = extern2
        }
    }

    sealed trait PartialExternSingleton[ET <: plugin.Extern.Tagged] extends PartialExtern with PartialExternRefSingleton[ET] {
      import extern.given

      override type Implementation = extern.Implementation
      override def implementationCodec: PartialESExprCodec[Implementation] =
        PartialESExprCodecSingleton[Implementation](plugin.pluginId)
        
      override def implementationEnvTag: EnvironmentTag[Implementation] = summon[EnvironmentTag[Implementation]]

      override def loadExtern(options: PlatformOptions)(id: String): OptionT[[A] =>> ZIO[PluginEnv, E, A], ZEnvironment[extern.Implementation]] =
        extern.loadExtern(options)(id).map(ZEnvironment.apply)
    }

    object PartialExternSingleton {
      def apply(extern2: plugin.Extern.Tagged): PartialExternSingleton[extern2.type] =
        new PartialExternSingleton[extern2.type] {
          override val extern: extern2.type = extern2
        }
    }

  }

  private final class Union[E >: PluginError](val aSet: PlatformPluginSet[E], val bSet: PlatformPluginSet[E]) extends PlatformPluginSet[E] {
    override val pluginIds: Set[String] = aSet.pluginIds | bSet.pluginIds

    override type PlatformOptions = (aSet.PlatformOptions, bSet.PlatformOptions)

    override def optionDecoder: PluginSetUtil.PartialOptionDecoder[PlatformOptions] =
      PluginSetUtil.PartialOptionDecoderUnion[aSet.PlatformOptions, bSet.PlatformOptions]


    override val externFunction: PartialExternUnion[aSet.externFunction.type, bSet.externFunction.type] =
      PartialExternUnion(aSet.externFunction, bSet.externFunction)

    override val externRecord: PartialExternRefUnion[aSet.externRecord.type, bSet.externRecord.type] =
      PartialExternRefUnion(aSet.externRecord, bSet.externRecord) 

    override def emitter[Ctx <: ContextIncluding]: TubeEmitterSet[E, Ctx] =
      TubeEmitterSet.Union(aSet.emitter, bSet.emitter)

    override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[TubeLoaderName, TubeLoader[Ctx]] = Map.empty


    sealed trait PartialExternRefUnion[EA <: aSet.PartialExternRef, EB <: bSet.PartialExternRef] extends PartialExternRef {
      val externA: EA
      val externB: EB


      override type Reference = externA.Reference & externB.Reference

      override def referenceCodec: PartialESExprCodec[Reference] =
        PartialESExprCodecUnion[externA.Reference, externB.Reference]

      override def referenceEnvTag: EnvironmentTag[Reference] = summon

      override def defineReference(options: PlatformOptions)(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, ZEnvironment[Reference]] =
        for
          externA <- externA.defineReference(options._1)(definitionInfo)
          externB <- externB.defineReference(options._2)(definitionInfo)
        yield externA ++ externB

    }

    object PartialExternRefUnion {
      def apply(ea: aSet.PartialExternRef, eb: bSet.PartialExternRef): PartialExternRefUnion[ea.type, eb.type] =
        new PartialExternRefUnion[ea.type, eb.type] {
          override val externA: ea.type = ea
          override val externB: eb.type = eb
        }
    }
    

    sealed trait PartialExternUnion[EA <: aSet.PartialExtern, EB <: bSet.PartialExtern] extends PartialExtern with PartialExternRefUnion[EA, EB] {
      override type Implementation = externA.Implementation & externB.Implementation

      override def implementationCodec: PartialESExprCodec[Implementation] =
        PartialESExprCodecUnion[externA.Implementation, externB.Implementation]

      override def implementationEnvTag: EnvironmentTag[Implementation] = summon

      override def loadExtern(options: PlatformOptions)(id: String): OptionT[[A] =>> ZIO[PluginEnv, E, A], ZEnvironment[Implementation]] =
        for
          externA <- externA.loadExtern(options._1)(id)
          externB <- externB.loadExtern(options._2)(id)
        yield externA ++ externB
    }

    object PartialExternUnion {
      def apply(ea: aSet.PartialExtern, eb: bSet.PartialExtern): PartialExternUnion[ea.type, eb.type] =
        new PartialExternUnion[ea.type, eb.type] {
          override val externA: ea.type = ea
          override val externB: eb.type = eb
        }
    }
  }

  def union[E >: PluginError](a: PlatformPluginSet[E], b: PlatformPluginSet[E]): PlatformPluginSet[E] =
    (a, b) match {
      case (_: PlatformPluginSet.Empty[E], _) => b
      case (_, _: PlatformPluginSet.Empty[E]) => a
      case _ => Union(a, b)
    }

}
