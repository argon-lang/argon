package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.ArTubeC
import dev.argon.esexpr.{Dictionary, ESExpr, ESExprCodec, ESExprTag}
import dev.argon.options.*
import dev.argon.io.*
import dev.argon.plugin.executor.TestExecutor
import dev.argon.util.{*, given}
import zio.*

import java.io.IOException

sealed abstract class Plugin[R <: CompEnv, E >: CompError] {
  val pluginId: String

  type Options
  type OutputOptions
  type Output

  given optionDecoder: OptionDecoder[R, E, Options]
  given outputOptionsDecoder: OptionDecoder[R, E, OutputOptions]
  given outputHandler: OutputHandler[R, E, Output]


  val externMethod: Extern
  val externFunction: Extern
  val externClassConstructor: Extern


  type PluginForPlatforms = Plugin[R, E] {
    type Options = Plugin.this.Options
    val externMethod: Plugin.this.externMethod.type
    val externFunction: Plugin.this.externFunction.type
    val externClassConstructor: Plugin.this.externClassConstructor.type
  }

  def emitTube[CtxPlugin <: PluginForPlatforms]
  (context: PluginContext[R, E, CtxPlugin])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  (options: OutputOptions)
  : context.Comp[Output]


  def loadExternMethod
  (options: Options)
  (id: String)
  : ZIO[R, E, Option[externMethod.Implementation]]


  def loadExternFunction
  (options: Options)
  (id: String)
  : ZIO[R, E, Option[externFunction.Implementation]]

  def loadExternClassConstructor
  (options: Options)
  (id: String)
  : ZIO[R, E, Option[externClassConstructor.Implementation]]

  def testExecutor: Option[TestExecutor[R, E, Options, Output]]

  def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, this.type]]
}

object Plugin {
  type WithId[S <: String] = { val pluginId: S }
}

sealed trait PlatformPluginSet[R, E] {
  def platformIds: List[String]

  type Options
  given optionDecoder: OptionDecoder[R, E, Options]


  val externMethod: Extern
  val externFunction: Extern
  val externClassConstructor: Extern

  def loadExternMethod
  (options: Options)
    (id: String)
  : ZIO[R, E, Option[externMethod.Implementation]]


  def loadExternFunction
  (options: Options)
    (id: String)
  : ZIO[R, E, Option[externFunction.Implementation]]

  def loadExternClassConstructor
  (options: Options)
    (id: String)
  : ZIO[R, E, Option[externClassConstructor.Implementation]]
}

abstract class PlatformPlugin[R <: CompEnv, E >: CompError] extends Plugin[R, E] with PlatformPluginSet[R, E] {
  override final def platformIds: List[String] = List(pluginId)
}

final class PluginSet[R, E](val partial: PluginSet.PartialPlugin[R, E]) extends PlatformPluginSet[R, E] {
  override def platformIds: List[String] = partial.platformIds

  override type Options = partial.Options

  override def optionDecoder: OptionDecoder[R, E, Options] =
    new OptionDecoder[R, E, Options] {
      override lazy val tags: Set[ESExprTag] = summon[ESExprCodec[Dictionary[ESExpr]]].tags
      override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, Options] =
        summon[ESExprCodec[Dictionary[ESExpr]]].decode(value)
          .flatMap { dict =>
            partial.optionDecoder.decode(resFactory)(dict.dict)
          }
    }

  override val externMethod: partial.externMethod.ExternType = partial.externMethod.toExtern
  override val externFunction: partial.externFunction.ExternType = partial.externFunction.toExtern
  override val externClassConstructor: partial.externClassConstructor.ExternType = partial.externClassConstructor.toExtern

  override def loadExternMethod(options: partial.Options)(id: String): ZIO[R, E, Option[externMethod.Implementation]] =
    partial.loadExternMethod(options)(id)

  override def loadExternFunction(options: partial.Options)(id: String): ZIO[R, E, Option[externFunction.Implementation]] =
    partial.loadExternFunction(options)(id)

  override def loadExternClassConstructor(options: partial.Options)(id: String): ZIO[R, E, Option[externClassConstructor.Implementation]] =
    partial.loadExternClassConstructor(options)(id)
}

object PluginSet {
  sealed trait PartialPlugin[R, E] {
    def platformIds: List[String]

    type Options <: Tuple
    def optionDecoder: PartialOptionDecoder[R, E, Options]


    val externMethod: PartialExtern
    val externFunction: PartialExtern
    val externClassConstructor: PartialExtern

    def loadExternMethod
    (options: Options)
      (id: String)
    : ZIO[R, E, Option[externMethod.Implementation]]


    def loadExternFunction
    (options: Options)
      (id: String)
    : ZIO[R, E, Option[externFunction.Implementation]]

    def loadExternClassConstructor
    (options: Options)
      (id: String)
    : ZIO[R, E, Option[externClassConstructor.Implementation]]
  }

  trait PartialOptionDecoder[R, E, Options] {
    def decode(resFactory: ResourceFactory[R, E])(value: Map[String, ESExpr]): Either[String, Options]
  }

  trait PartialESExprCodec[A] {
    def encode(value: A): Map[String, ESExpr]
    def decode(kwargs: Map[String, ESExpr]): Either[String, A]

    def toCodec: ESExprCodec[A] =
      new ESExprCodec[A] {
        override lazy val tags: Set[ESExprTag] = summon[ESExprCodec[Dictionary[ESExpr]]].tags

        override def encode(value: A): ESExpr =
          summon[ESExprCodec[Dictionary[ESExpr]]]
            .encode(Dictionary(PartialESExprCodec.this.encode(value)))

        override def decode(expr: ESExpr): Either[String, A] =
          summon[ESExprCodec[Dictionary[ESExpr]]].decode(expr)
            .flatMap { dict =>
              PartialESExprCodec.this.decode(dict.dict)
            }
      }
  }

  object PartialESExprCodecNil extends PartialESExprCodec[EmptyTuple] {
    override def encode(value: EmptyTuple): Map[String, ESExpr] = Map.empty

    // Ignore any platforms we don't care about
    override def decode(kwargs: Map[String, ESExpr]): Either[String, EmptyTuple] = Right(EmptyTuple)
  }

  final class PartialESExprCodecCons[H: ESExprCodec, T <: Tuple: PartialESExprCodec](headId: String) extends PartialESExprCodec[H *: T] {
    override def encode(value: H *: T): Map[String, ESExpr] =
      val (h *: t) = value
      summon[PartialESExprCodec[T]].encode(t) + (headId -> summon[ESExprCodec[H]].encode(h))
    end encode

    override def decode(kwargs: Map[String, ESExpr]): Either[String, H *: T] =
      for
        headExpr <- kwargs.get(headId).toRight("Missing value for platform")
        h <- summon[ESExprCodec[H]].decode(headExpr)
        t <- summon[PartialESExprCodec[T]].decode(kwargs.removed(headId))
      yield h *: t

  }

  trait PartialExtern {
    type Implementation <: Tuple
    given implementationCodec: PartialESExprCodec[Implementation]

    type Reference <: Tuple
    given referenceCodec: PartialESExprCodec[Reference]

    type ExternType = Extern {
      type Implementation = PartialExtern.this.Implementation
      type Reference = PartialExtern.this.Reference
    }

    def toExtern: ExternType =
      new Extern {
        override type Implementation = PartialExtern.this.Implementation
        override def implementationCodec: ESExprCodec[Implementation] =
          PartialExtern.this.implementationCodec.toCodec

        override type Reference = PartialExtern.this.Reference

        override def referenceCodec: ESExprCodec[Reference] =
          PartialExtern.this.referenceCodec.toCodec
      }
  }

  object PartialExternNil extends PartialExtern {
    override type Implementation = EmptyTuple
    override def implementationCodec: PartialESExprCodec[EmptyTuple] = PartialESExprCodecNil

    override type Reference = EmptyTuple
    override def referenceCodec: PartialESExprCodec[EmptyTuple] = PartialESExprCodecNil
  }

  final class PartialExternCons[H <: Extern, T <: PartialExtern](headId: String, val h: H, val t: T) extends PartialExtern {
    import h.given
    import t.given

    override type Implementation = h.Implementation *: t.Implementation
    override def implementationCodec: PartialESExprCodec[Implementation] =
      PartialESExprCodecCons[h.Implementation, t.Implementation](headId)

    override type Reference = h.Reference *:  t.Reference
    override def referenceCodec: PartialESExprCodec[Reference] =
      PartialESExprCodecCons[h.Reference, t.Reference](headId)
  }

  final class PartialPluginNil[R, E] extends PartialPlugin[R, E] {
    override def platformIds: List[String] = Nil

    override type Options = EmptyTuple

    override def optionDecoder: PartialOptionDecoder[R, E, EmptyTuple] =
      new PartialOptionDecoder[R, E, EmptyTuple] {
        override def decode(resFactory: ResourceFactory[R, E])(value: Map[String, ESExpr]): Either[String, EmptyTuple] =
          if value.nonEmpty then
            Left("Unexpected platform options were specified")
          else
            Right(EmptyTuple)
      }

    override val externMethod: PartialExternNil.type = PartialExternNil
    override val externFunction: PartialExternNil.type = PartialExternNil
    override val externClassConstructor: PartialExternNil.type = PartialExternNil

    override def loadExternMethod(options: EmptyTuple)(id: String): ZIO[Any, Nothing, Option[externMethod.Implementation]] =
      ZIO.some(EmptyTuple)

    override def loadExternFunction(options: EmptyTuple)(id: String): ZIO[Any, Nothing, Option[externFunction.Implementation]] =
      ZIO.some(EmptyTuple)

    override def loadExternClassConstructor(options: EmptyTuple)(id: String): ZIO[Any, Nothing, Option[externClassConstructor.Implementation]] =
      ZIO.some(EmptyTuple)
  }

  final class PartialPluginCons[R <: CompEnv, E >: CompError](val platformPlugin: PlatformPlugin[R, E], val tail: PartialPlugin[R, E]) extends PartialPlugin[R, E] {
    override def platformIds: List[String] = platformPlugin.pluginId :: tail.platformIds

    override type Options = platformPlugin.Options *: tail.Options

    override def optionDecoder: PartialOptionDecoder[R, E, Options] =
      new PartialOptionDecoder[R, E, Options] {
        override def decode(resFactory: ResourceFactory[R, E])(value: Map[String, ESExpr]): Either[String, Options] =
          for
            headExpr <- value.get(platformPlugin.pluginId).toRight("Missing options for platform")
            h <- platformPlugin.optionDecoder.decode(resFactory)(headExpr)
            t <- tail.optionDecoder.decode(resFactory)(value.removed(platformPlugin.pluginId))
          yield h *: t
      }

    override val externMethod: PartialExternCons[platformPlugin.externMethod.type, tail.externMethod.type] =
      PartialExternCons(platformPlugin.pluginId, platformPlugin.externMethod, tail.externMethod)

    override val externFunction: PartialExternCons[platformPlugin.externFunction.type, tail.externFunction.type] =
      PartialExternCons(platformPlugin.pluginId, platformPlugin.externFunction, tail.externFunction)

    override val externClassConstructor: PartialExternCons[platformPlugin.externClassConstructor.type, tail.externClassConstructor.type] =
      PartialExternCons(platformPlugin.pluginId, platformPlugin.externClassConstructor, tail.externClassConstructor)

    override def loadExternMethod(options: Options)(id: String): ZIO[R, E, Option[externMethod.Implementation]] =
      val (hOptions *: tOptions) = options
      platformPlugin.loadExternMethod(hOptions)(id).flatMap {
        case Some(h) =>
          tail.loadExternMethod(tOptions)(id).map(_.map(t => h *: t))

        case None => ZIO.none
      }
    end loadExternMethod

    override def loadExternFunction(options: Options)(id: String): ZIO[R, E, Option[externFunction.Implementation]] =
      val (hOptions *: tOptions) = options
      platformPlugin.loadExternFunction(hOptions)(id).flatMap {
        case Some(h) =>
          tail.loadExternFunction(tOptions)(id).map(_.map(t => h *: t))

        case None => ZIO.none
      }
    end loadExternFunction

    override def loadExternClassConstructor(options: Options)(id: String): ZIO[R, E, Option[externClassConstructor.Implementation]] =
      val (hOptions *: tOptions) = options
      platformPlugin.loadExternClassConstructor(hOptions)(id).flatMap {
        case Some(h) =>
          tail.loadExternClassConstructor(tOptions)(id).map(_.map(t => h *: t))

        case None => ZIO.none
      }
    end loadExternClassConstructor
  }


}

abstract class CompositePlugin[R <: CompEnv, E >: CompError, PlatformPlugins <: PlatformPluginSet[R, E]] extends Plugin[R, E] {
  val platformPlugins: PlatformPlugins

  override final type Options = platformPlugins.Options
  override final def optionDecoder: OptionDecoder[R, E, platformPlugins.Options] = platformPlugins.optionDecoder

  override final val externMethod: platformPlugins.externMethod.type = platformPlugins.externMethod
  override final val externFunction: platformPlugins.externFunction.type = platformPlugins.externFunction
  override final val externClassConstructor: platformPlugins.externClassConstructor.type = platformPlugins.externClassConstructor

  override final def loadExternMethod(options: Options)(id: String): ZIO[R, E, Option[externMethod.Implementation]] =
    platformPlugins.loadExternMethod(options)(id)

  override final def loadExternFunction(options: Options)(id: String): ZIO[R, E, Option[externFunction.Implementation]] =
    platformPlugins.loadExternFunction(options)(id)

  override final def loadExternClassConstructor(options: Options)(id: String): ZIO[R, E, Option[externClassConstructor.Implementation]] =
    platformPlugins.loadExternClassConstructor(options)(id)
}
