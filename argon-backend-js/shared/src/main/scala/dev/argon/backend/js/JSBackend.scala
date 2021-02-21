package dev.argon.backend.js

import cats.{Applicative, Id}
import dev.argon.compiler._
import cats.implicits._
import dev.argon.backend._
import dev.argon.backend.js.JSBackend.{JSOutputId, PlatformId}
import dev.argon.compiler.core.{ArModule, Context}
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.options.{FileList, OptionDecoder, OptionInfo, Options, OptionsHandler, SingleFile}
import dev.argon.compiler.output.TextBuildArtifact
import zio._
import dev.argon.io.fileio.FileIO
import dev.argon.io.fileio.ZipRead
import dev.argon.util.{MaybeBlocking, ProtoBufCodecs, ValueCache}
import shapeless.{::, HNil}
import zio.stream.ZStream


sealed abstract class JSBackend extends Backend {

  final override val id: String = PlatformId
  final override val name: String = "JavaScript"
  final override val platformIds: Set[String] = Set(PlatformId)


  override type BackendOptionID = JSBackendOptionID
  final override val backendOptions: OptionsHandler[JSBackendOptionID, Id] = new OptionsHandler[JSBackendOptionID, Id] {


    override type OptRepr[A[_]] = A[FileList] :: A[Option[SingleFile]] :: A[Option[SingleFile]] :: HNil


    override def ids: OptRepr[Lambda[X => BackendOptionID { type ElementType = X }]] =
      JSBackendOptionID.Externs :: JSBackendOptionID.InjectAfter :: JSBackendOptionID.InjectBefore :: HNil


    override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: OptionsHandler.CombineFunction[JSBackendOptionID, A, B, C, F]): F[OptRepr[C]] = {
      import dev.argon.options.OptionCombineHelper._
      combineHLists(ids, lista, listb)(f)
    }

    override def reprToOptions[A[_]](list: OptRepr[A]): Options[A, JSBackendOptionID] =
      Options.fromFunction(new Options.OptionValueFunction[A, JSBackendOptionID] {
        override def apply[E](id: JSBackendOptionID { type ElementType = E }): A[E] = id match {
          case JSBackendOptionID.Externs => list.head
          case JSBackendOptionID.InjectBefore => list.tail.head
          case JSBackendOptionID.InjectAfter => list.tail.tail.head
        }
      })

    override def info: Options[OptionInfo, JSBackendOptionID] =
      Options.fromFunction(new Options.OptionValueFunction[OptionInfo, JSBackendOptionID] {
        override def apply[E](id: JSBackendOptionID { type ElementType = E }): OptionInfo[E] = id match {
          case JSBackendOptionID.Externs =>
            OptionInfo(
              name = "js.extern",
              description = "JS module defining extern function implementations",
              defaultValue = new FileList(Seq.empty),
            )

          case JSBackendOptionID.InjectBefore =>
            OptionInfo(
              name = "js.inject.before",
              description = "JS code injected at the top of the module",
              defaultValue = None,
            )

          case JSBackendOptionID.InjectAfter =>
            OptionInfo(
              name = "inject.after",
              description = "JS code injected at the bottom of the module",
              defaultValue = None,
            )
        }
      })

    override def decoder: Options[OptionDecoder, JSBackendOptionID] = {
      import dev.argon.options.OptionDecoderHelper._
      reprToOptions(createDecoders[OptRepr[OptionDecoder]])
    }
  }


  override type OutputOptionID = JSOutputOptionID


  override val outputOptions: OptionsHandler[JSOutputOptionID, Lambda[X => SingleFile]] = new OptionsHandler[JSOutputOptionID, Lambda[X => SingleFile]] {
    override type OptRepr[A[_]] = A[TextBuildArtifact] :: HNil

    override def ids: OptRepr[Lambda[X => JSOutputOptionID { type ElementType = X }]] =
      JSOutputOptionID.JSModule :: HNil

    override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: OptionsHandler.CombineFunction[JSOutputOptionID, A, B, C, F]): F[OptRepr[C]] = {
      import dev.argon.options.OptionCombineHelper._
      combineHLists(ids, lista, listb)(f)
    }

    override def reprToOptions[A[_]](list: OptRepr[A]): Options[A, JSOutputOptionID] =
      Options.fromFunction(new Options.OptionValueFunction[A, JSOutputOptionID] {
        override def apply[E](id: JSOutputOptionID { type ElementType = E }): A[E] = id match {
          case JSOutputOptionID.JSModule => list.head
        }
      })

    override def info: Options[OptionInfo, JSOutputOptionID] =
      Options.fromFunction(new Options.OptionValueFunction[OptionInfo, JSOutputOptionID] {
        override def apply[E](id: JSOutputOptionID { type ElementType = E }): OptionInfo[E] = id match {
          case JSOutputOptionID.JSModule => OptionInfo("output.js.module", "The compiled JS module file")
        }
      })

    override def decoder: Options[Lambda[X => OptionDecoder[SingleFile]], JSOutputOptionID] = {
      import dev.argon.options.OptionDecoderHelper._
      reprToOptions(createDecoders[OptRepr[Lambda[X => OptionDecoder[SingleFile]]]])
    }
  }


  override def moduleLoaders(options: Options[Id, JSBackendOptionID]): Seq[ModuleLoader[Context.Aux[JSBackend.this.type]]] = Seq()

  override type TExternHandler = JSExternHandler
}

object JSBackend {

  val PlatformId = "js"
  val JSOutputId = "js"


  def make: URIO[ZipRead with MaybeBlocking with FileIO, JSBackend] = for {
    env <- ZIO.environment[FileIO with MaybeBlocking]
  } yield (new JSBackend {

    type TSelf = this.type

    override def externHandler(options: Options[Id, JSBackendOptionID]): UIO[JSExternHandler] =
      JSExternHandler.make(options).provide(env)

    override def emitModule(options: Options[Id, JSBackendOptionID])(context: Context.Aux[this.type])(module: ArModule[context.type, DeclarationPayloadSpecifier]): Options[Id, JSOutputOptionID] =
      Options.fromFunction(new Options.OptionValueFunction[Id, JSOutputOptionID] {
        override def apply[E](id: JSOutputOptionID { type ElementType = E }): Id[E] = id match {
          case JSOutputOptionID.JSModule =>
            new TextBuildArtifact {
              override def asStringStream: CompStream[String] = {
                val inject = JSInjectCode(
                  before = options.get(JSBackendOptionID.InjectBefore),
                  after = options.get(JSBackendOptionID.InjectAfter),
                )

                val emitter = JSEmitter[TSelf](context, inject)
                ZStream.unwrap(
                  emitter.emitModule(module)
                    .provide(env)
                    .map { jsModule =>
                      JSAst.writeModule(jsModule).toZStream
                    }
                )
              }
            }
        }
      })

  } : JSBackend)

}
