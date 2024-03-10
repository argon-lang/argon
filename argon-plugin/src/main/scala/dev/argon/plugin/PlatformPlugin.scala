package dev.argon.plugin

import dev.argon.util.given
import cats.data.OptionT
import dev.argon.compiler.*
import dev.argon.esexpr.ESExprCodec
import dev.argon.options.{OptionDecoder, OutputHandler}
import dev.argon.plugin.PlatformPluginSet.Empty
import dev.argon.plugin.PluginSetUtil.*
import zio.*
import zio.interop.catz.core.given

abstract class PlatformPlugin extends ExternContext {
  val pluginId: String

  type PlatformOptions[E >: PluginError]

  given optionDecoder[E >: PluginError]: OptionDecoder[E, PlatformOptions[E]]


  val externFunction: Extern.Tagged


  type ContextIncluding = Context {
    type Env <: PluginEnv
    type Error >: PluginError
    val implementations: {
      type ExternFunctionImplementation <: ZEnvironment[externFunction.Implementation]
      type FunctionReference <: ZEnvironment[externFunction.Reference]
    }
  }

  type ContextOnlyIncluding = Context {
    type Env <: PluginEnv
    type Error >: PluginError
    val implementations: {
      type ExternFunctionImplementation = ZEnvironment[externFunction.Implementation]
      type FunctionReference = ZEnvironment[externFunction.Reference]
    }
  }

  def emitter[Ctx <: ContextIncluding]: Option[TubeEmitter[Ctx]]
  def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[String, TubeLoader[Ctx]]

}

sealed trait PlatformPluginSet {
  val pluginIds: Set[String]

  type PlatformOptions[E >: PluginError]

  given optionDecoder[E >: PluginError]: PluginSetUtil.PartialOptionDecoder[E, PlatformOptions[E]]



  type ContextIncluding = Context {
    type Env <: PluginEnv
    type Error >: PluginError
    val implementations: {
      type ExternFunctionImplementation <: ZEnvironment[externFunction.Implementation]
      type FunctionReference <: ZEnvironment[externFunction.Reference]
    }
  }

  type ContextOnlyIncluding = Context {
    type Env <: PluginEnv
    type Error >: PluginError
    val implementations: {
      type ExternFunctionImplementation = ZEnvironment[externFunction.Implementation]
      type FunctionReference = ZEnvironment[externFunction.Reference]
    }
  }

  val externFunction: PartialExtern


  def emitter[Ctx <: ContextIncluding]: TubeEmitterSet[Ctx]
  def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[TubeLoaderName, TubeLoader[Ctx]]



  sealed trait PartialExtern {
    type Implementation
    given implementationCodec: PartialESExprCodec[Implementation]
    given implementationEnvTag: EnvironmentTag[Implementation]

    def loadExtern[E >: PluginError]
    (options: PlatformOptions[E])
    (id: String)
    : OptionT[[A] =>> ZIO[PluginEnv, E, A], ZEnvironment[Implementation]]



    type Reference
    given referenceCodec: PartialESExprCodec[Reference]
    given referenceEnvTag: EnvironmentTag[Reference]

    def defineReference[E >: PluginError]
    (options: PlatformOptions[E])
    (definitionInfo: DefinitionInfo)
    : ZIO[PluginEnv, E, ZEnvironment[Reference]]


    def asExtern(externContext: ExternContext { type PlatformOptions[E >: PluginError] = PlatformPluginSet.this.PlatformOptions[E] }): externContext.Extern {
      type Implementation = ZEnvironment[PartialExtern.this.Implementation]
      type Reference = ZEnvironment[PartialExtern.this.Reference]
    } =
      new externContext.Extern {
        override type Implementation = ZEnvironment[PartialExtern.this.Implementation]

        override def implementationCodec: ESExprCodec[Implementation] =
          PartialExtern.this.implementationCodec.toCodec

        override def loadExtern[E >: PluginError](options: externContext.PlatformOptions[E])(id: String): OptionT[[A] =>> ZIO[PluginEnv, E, A], Implementation] =
          PartialExtern.this.loadExtern(options)(id)

        override type Reference = ZEnvironment[PartialExtern.this.Reference]

        override def referenceCodec: ESExprCodec[Reference] =
          PartialExtern.this.referenceCodec.toCodec

        override def defineReference[E >: PluginError](options: externContext.PlatformOptions[E])(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, Reference] =
          PartialExtern.this.defineReference(options)(definitionInfo)
      }
  }

}

private[plugin] object PlatformPluginSet {
  object Empty extends PlatformPluginSet {
    override val pluginIds: Set[String] = Set.empty

    override type PlatformOptions[E >: PluginError] = Unit

    override def optionDecoder[E >: PluginError]: PluginSetUtil.PartialOptionDecoder[E, Unit] =
      PluginSetUtil.PartialOptionDecoderEmpty[E]()


    override val externFunction: PartialExternEmpty =
      PartialExternEmpty()


    override def emitter[Ctx <: ContextIncluding]: TubeEmitterSet[Ctx] =
      TubeEmitterSet.Empty()

    override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[TubeLoaderName, TubeLoader[Ctx]] = Map.empty

    final class PartialExternEmpty extends PartialExtern {
      override type Implementation = Any
      override def implementationCodec: PartialESExprCodec[Any] = PartialESExprCodecEmpty()
      override def implementationEnvTag: EnvironmentTag[Any] = summon

      override def loadExtern[E >: PluginError](options: PlatformOptions[E])(id: String): OptionT[[A] =>> ZIO[PluginEnv, E, A], ZEnvironment[Implementation]] =
        OptionT.some(ZEnvironment.empty)

      override type Reference = Any
      override def referenceCodec: PartialESExprCodec[Any] = PartialESExprCodecEmpty()
      override def referenceEnvTag: EnvironmentTag[Any] = summon

      override def defineReference[E >: PluginError](options: PlatformOptions[E])(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, ZEnvironment[Any]] =
        ZIO.succeed(ZEnvironment.empty)
    }
  }

  final class Singleton(val plugin: PlatformPlugin) extends PlatformPluginSet {
    override val pluginIds: Set[String] = Set(plugin.pluginId)

    override type PlatformOptions[E >: PluginError] = plugin.PlatformOptions[E]

    override def optionDecoder[E >: PluginError]: PluginSetUtil.PartialOptionDecoder[E, PlatformOptions[E]] =
      PluginSetUtil.PartialOptionDecoderSingleton[E, PlatformOptions[E]](plugin.pluginId)


    override val externFunction: PartialExternSingleton[plugin.externFunction.type] =
      PartialExternSingleton(plugin.externFunction)


    override def emitter[Ctx <: ContextIncluding]: TubeEmitterSet[Ctx] =
      plugin.emitter[Ctx] match {
        case Some(emitter) => TubeEmitterSet.Singleton(emitter, plugin.pluginId)
        case None => TubeEmitterSet.Empty()
      }


    override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[TubeLoaderName, TubeLoader[Ctx]] =
      plugin.tubeLoaders[Ctx].map { (k, v) => TubeLoaderName(plugin.pluginId, k) -> v }


    sealed trait PartialExternSingleton[ET <: plugin.Extern.Tagged] extends PartialExtern {
      val extern: ET

      override type Implementation = extern.Implementation
      override def implementationCodec: PartialESExprCodec[Implementation] =
        PartialESExprCodecSingleton[Implementation](plugin.pluginId)
      override def implementationEnvTag: EnvironmentTag[Implementation] = summon

      override def loadExtern[E >: PluginError](options: PlatformOptions[E])(id: String): OptionT[[A] =>> ZIO[PluginEnv, E, A], ZEnvironment[extern.Implementation]] =
        extern.loadExtern(options)(id).map(ZEnvironment.apply)

      override type Reference = extern.Reference

      override def referenceCodec: PartialESExprCodec[Reference] =
        PartialESExprCodecSingleton[Reference](plugin.pluginId)

      override def referenceEnvTag: EnvironmentTag[Reference] = summon

      override def defineReference[E >: PluginError](options: PlatformOptions[E])(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, ZEnvironment[extern.Reference]] =
        extern.defineReference(options)(definitionInfo).map(ZEnvironment.apply)
    }

    object PartialExternSingleton {
      def apply(extern2: plugin.Extern.Tagged): PartialExternSingleton[extern2.type] =
        new PartialExternSingleton[extern2.type] {
          override val extern: extern2.type = extern2
        }
    }

  }

  private final class Union(val aSet: PlatformPluginSet, val bSet: PlatformPluginSet) extends PlatformPluginSet {
    override val pluginIds: Set[String] = aSet.pluginIds | bSet.pluginIds

    override type PlatformOptions[E >: PluginError] = (aSet.PlatformOptions[E], bSet.PlatformOptions[E])

    override def optionDecoder[E >: PluginError]: PluginSetUtil.PartialOptionDecoder[E, PlatformOptions[E]] =
      PluginSetUtil.PartialOptionDecoderUnion[E, aSet.PlatformOptions[E], bSet.PlatformOptions[E]]


    override val externFunction: PartialExternUnion[aSet.externFunction.type, bSet.externFunction.type] =
      PartialExternUnion(aSet.externFunction, bSet.externFunction)


    override def emitter[Ctx <: ContextIncluding]: TubeEmitterSet[Ctx] =
      TubeEmitterSet.Union(aSet.emitter, bSet.emitter)

    override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[TubeLoaderName, TubeLoader[Ctx]] = Map.empty


    sealed trait PartialExternUnion[EA <: aSet.PartialExtern, EB <: bSet.PartialExtern] extends PartialExtern {
      val externA: EA
      val externB: EB

      override type Implementation = externA.Implementation & externB.Implementation

      override def implementationCodec: PartialESExprCodec[Implementation] =
        PartialESExprCodecUnion[externA.Implementation, externB.Implementation]

      override def implementationEnvTag: EnvironmentTag[Implementation] = summon

      override def loadExtern[E >: PluginError](options: PlatformOptions[E])(id: String): OptionT[[A] =>> ZIO[PluginEnv, E, A], ZEnvironment[Implementation]] =
        for
          externA <- externA.loadExtern(options._1)(id)
          externB <- externB.loadExtern(options._2)(id)
        yield externA ++ externB



      override type Reference = externA.Reference & externB.Reference

      override def referenceCodec: PartialESExprCodec[Reference] =
        PartialESExprCodecUnion[externA.Reference, externB.Reference]

      override def referenceEnvTag: EnvironmentTag[Reference] = summon

      override def defineReference[E >: PluginError](options: PlatformOptions[E])(definitionInfo: DefinitionInfo): ZIO[PluginEnv, E, ZEnvironment[Reference]] =
        for
          externA <- externA.defineReference(options._1)(definitionInfo)
          externB <- externB.defineReference(options._2)(definitionInfo)
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

  def union(a: PlatformPluginSet, b: PlatformPluginSet): PlatformPluginSet =
    (a, b) match {
      case (_: PlatformPluginSet.Empty.type, _) => b
      case (_, _: PlatformPluginSet.Empty.type) => a
      case _ => Union(a, b)
    }

}
