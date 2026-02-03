package dev.argon.backend

import dev.argon.backend.options.OptionValue
import dev.argon.compiler.{Context, ExternProvider, HasContext}
import dev.argon.tube.{ExternMap, SupportedPlatform}
import esexpr.{Dictionary, ESExpr}
import zio.*
import cats.syntax.all.*
import cats.data.OptionT
import zio.interop.catz.core.given

import java.io.IOException

object BackendExternProvider {
  type ProviderContext = Context {
    type Error >: BackendException | IOException
    val implementations: Context.ImplementationExterns {
      type TubeMetadata = (Seq[SupportedPlatform], Map[String, ESExpr])
      type ExternFunction = ExternMap
      type ExternMethod = ExternMap
    }
  }

  def make(ctx: ProviderContext)(supportedPlatforms: Set[String], tubeOptions: Map[String, Map[String, OptionValue[ctx.Error]]]): ZIO[Scope & ctx.Env & BackendProvider, ctx.Error, ExternProvider & HasContext[ctx.type]] =
    for
      backends <- loadBackends(supportedPlatforms ++ tubeOptions.keySet)
      loaderInfos <- ZIO.foreach(tubeOptions) { (backendId, optValues) =>
        for
          loaderInfo <- getLoaderInfo(backends(backendId), optValues)
        yield backendId -> loaderInfo
      }
    yield new ExternProvider {
      override val context: ctx.type = ctx

      override def getTubeMetadata: Comp[(Seq[SupportedPlatform], Map[String, ESExpr])] =
        for
          tubeMeta <- ZIO.foreach(loaderInfos) { (backendId, loaderInfo) =>
            loaderInfo.options
              .flatMap(loaderInfo.platformDataLoader.getTubeMetadata)
              .map(backendId -> _)
          }
        yield (
          supportedPlatforms.toSeq.map { platformId => SupportedPlatform(platformId) },
          tubeMeta
        )

      override def getExternFunction(name: String): Comp[Option[ExternMap]] =
        loaderInfos
          .toSeq
          .traverse { (backendId, loaderInfo) =>
            OptionT(loaderInfo.externLoader.flatMap(loader => loader.getExtern(name)))
              .filter(_.allowFunction)
              .map(externInfo => backendId -> externInfo.value)
          }
          .map { externs => ExternMap(Dictionary(externs.toMap)) }
          .value

      override def getExternMethod(name: String): Comp[Option[ExternMap]] =
        loaderInfos
          .toSeq
          .traverse { (backendId, loaderInfo) =>
            OptionT(loaderInfo.externLoader.flatMap(loader => loader.getExtern(name)))
              .filter(_.allowMethod)
              .map(externInfo => backendId -> externInfo.value)
          }
          .map { externs => ExternMap(Dictionary(externs.toMap)) }
          .value
    }

  private def loadBackends[E >: BackendException | IOException](backends: Set[String]): ZIO[Scope & BackendProvider, E, Map[String, Backend[E]]] =
    ZIO.foreach(backends.toSeq) { backendId =>
      ZIO.serviceWithZIO[BackendProvider](_.getBackendFactory(backendId))
        .flatMap { factory =>
          factory.load[E]
        }
        .map(backendId -> _)
    }.map(_.toMap)

  private final class LoaderInfo[E >: BackendException | IOException](
    val backend: Backend[E],
    val platformDataLoader: PlatformDataLoader[E],
    val options: IO[E, platformDataLoader.Options],
    val externLoader: IO[E, ExternLoader[E]],
  )

  private def getLoaderInfo[E >: BackendException | IOException](backend: Backend[E], optionValues: Map[String, OptionValue[E]]): ZIO[Scope, E, LoaderInfo[E]] =
    for
      options <- backend.platformDataLoader.optionParser.parse(optionValues).memoize
      scope <- ZIO.environment[Scope]
      externLoader <- options.flatMap(backend.platformDataLoader.externLoader).memoize
    yield LoaderInfo(
      backend = backend,
      platformDataLoader = backend.platformDataLoader,
      options = options,
      externLoader = externLoader.provideEnvironment(scope),
    )

}

