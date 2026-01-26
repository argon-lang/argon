package dev.argon.backend

import dev.argon.backend.scalaApi.{ScopedResource, StreamUtil}
import dev.argon.backend.scalaApi.ScopedResourceExtensions.*
import dev.argon.backend.options.{OptionParseFailure, OptionParser, OptionValue, OutputProvider}
import dev.argon.compiler.TubeName
import dev.argon.io.{BinaryResource, FileSystemResource}
import dev.argon.util.async.ErrorWrapper
import dev.argon.vm
import dev.argon.vm.resource.VmIrResource
import esexpr.{Dictionary, ESExpr}
import zio.*

import java.io.IOException

object ScalaApiBackendLoader {
  def loadScalaApiBackend[E >: BackendException | IOException, Outs](using ew: ErrorWrapper[E])(backendName: String)(backend: scalaApi.Backend[ew.EX, Outs]): UIO[Backend[E]] =
    for
      given Runtime[Any] <- ZIO.runtime[Any]
      apiPlatformDataLoader <- backend.platformDataLoader().flatMap { pdlFac =>
        pdlFac.create(
          new scalaApi.PlatformDataLoaderFactoryCallback[ew.EX, scalaApi.PlatformDataLoader[ew.EX, ?]] {
            override def call[Options](platformDataLoader: scalaApi.PlatformDataLoader[ew.EX, Options]): UIO[scalaApi.PlatformDataLoader[ew.EX, ?]] =
              ZIO.succeed(platformDataLoader)
          }
        )
      }
      apiCodeGen <- backend.codeGenerator().flatMap(codeGenFac => {
        codeGenFac.create(
          new scalaApi.CodeGeneratorFactoryCallback[ew.EX, Outs, scalaApi.CodeGenerator[ew.EX, ?, Outs]] {
            override def call[Options](codeGenerator: scalaApi.CodeGenerator[ew.EX, Options, Outs]): UIO[scalaApi.CodeGenerator[ew.EX, ?, Outs]] =
              ZIO.succeed(codeGenerator)
          }
        )
      })
    yield createBackend(backendName, backend, apiPlatformDataLoader, apiCodeGen)

  private def createBackend[E >: BackendException | IOException, TubeOpts, CodeGenOpts, Outs](using ew: ErrorWrapper[E], rt: Runtime[Any])(
    backendName: String,
    backend: scalaApi.Backend[ew.EX, Outs],
    apiPlatformDataLoader: scalaApi.PlatformDataLoader[ew.EX, TubeOpts],
    apiCodeGen: scalaApi.CodeGenerator[ew.EX, CodeGenOpts, Outs],
  ): Backend[E] =
    new Backend[E] {
      override type Output = Outs

      override def name: String = backendName

      override val platformDataLoader: PlatformDataLoader[E] =
        new PlatformDataLoader[E] {
          override type Options = TubeOpts

          override def optionParser: OptionParser[E, TubeOpts] =
            unwrapOptionParser(apiPlatformDataLoader.optionParser())

          override def getTubeMetadata(options: TubeOpts): IO[E, ESExpr] =
            ErrorWrapper.unwrapEffect(apiPlatformDataLoader.getTubeMetadata(options))

          override def externLoader(options: TubeOpts): ZIO[Scope, E, ExternLoader[E]] =
            ErrorWrapper.unwrapEffect(
                apiPlatformDataLoader.externLoader(options)
                  .flatMap(_.toScopeIO)
              )
              .map { extLoader =>
                new ExternLoader[E] {
                  override def getExtern(name: String): IO[E, Option[scalaApi.ExternInfo]] =
                    ErrorWrapper.unwrapEffect(extLoader.getExtern(name))
                }
              }
        }


      override val codeGenerator: CodeGenerator[E, Outs] =
        apiCodeGen match {
          case scalaApi.CodeGenerator.Library(libCodeGen) =>
            new CodeGenerator.LibraryCodeGenerator[E, Outs] {
              override type Options = CodeGenOpts

              override def optionParser: OptionParser[E, CodeGenOpts] =
                unwrapOptionParser(libCodeGen.optionParser())

              override def outputProvider: OutputProvider[E, Outs] =
                unwrapOutputProvider(libCodeGen.outputProvider())

              override def codegen(options: CodeGenOpts, program: VmIrResource[E], libraries: Seq[VmIrResource[E]]): ZIO[Scope, E, Outs] =
                libCodeGen.codegen(
                    options,
                    vmIrToApi(program),
                    libraries.map(vmIrToApi[E]),
                  )
                  .catchAll(e => ZIO.failCause(ew.unwrap(e)))

            }
        }
    }


  private def vmIrToApi[E >: IOException](res: VmIrResource[E])(using ew: ErrorWrapper[E]): scalaApi.VmIrTube[ew.EX] =
    new scalaApi.VmIrTube[ew.EX] {
      override def stream(): UIO[ScopedResource[ew.EX, scalaApi.Stream[ew.EX, vm.TubeFileEntry]]] = {
        StreamUtil.fromZStreamScoped(ErrorWrapper.wrapStream(res.decoded))
      }
    }

  private def unwrapOptionParser[E >: IOException, Options](using ew: ErrorWrapper[E], rt: Runtime[Any])(op: UIO[scalaApi.options.OptionParser[ew.EX, Options]]): OptionParser[E, Options] =
    new OptionParser[E, Options] {
      override def parse(options: Map[String, OptionValue[E]]): IO[OptionParseFailure, Options] =
        op.flatMap(_.parse(Dictionary(options.view.mapValues(wrapOptionValue).toMap)))
          .mapError(OptionParseFailure.apply)

      private def wrapOptionValue(value: OptionValue[E]): scalaApi.options.OptionValue[ew.EX] =
        value match {
          case OptionValue.Single(a) =>
            scalaApi.options.OptionValue.SingleValue(wrapOptionValueAtom(a))
            
          case OptionValue.ManyValues(as) =>
            scalaApi.options.OptionValue.ManyValues(
              wrapOptionValueAtom(as.head),
              as.tail.map(wrapOptionValueAtom)
            )
        }

      private def wrapOptionValueAtom(value: OptionValue.Atom[E]): scalaApi.options.OptionValueAtom[ew.EX] =
        value match {
          case OptionValue.Atom.String(s) => scalaApi.options.OptionValueAtom.String(s)
          case OptionValue.Atom.Bool(b) => scalaApi.options.OptionValueAtom.Bool(b)
          case OptionValue.Atom.BinaryResource(res) => scalaApi.options.OptionValueAtom.BinaryResource(BinaryResourceWrap.wrap(res))
          case OptionValue.Atom.DirectoryResource(res) => scalaApi.options.OptionValueAtom.DirectoryResource(DirectoryResourceWrap.wrap(res))
        }
    }

  private def unwrapOutputProvider[E >: IOException, Output](using ew: ErrorWrapper[E])(op: UIO[scalaApi.options.OutputProvider[ew.EX, Output]]): OutputProvider[E, Output] =
    new OutputProvider[E, Output] {
      override def outputs(output: Output): IO[E, Map[String, FileSystemResource[E, BinaryResource]]] =
        op.flatMap(_.resources(output))
          .map { _.dict.view.mapValues(unwrapOutputValue).toMap }

      private def unwrapOutputValue(value: scalaApi.options.OutputValue[ew.EX]): FileSystemResource[E, BinaryResource] =
        value match {
          case scalaApi.options.OutputValue.BinaryResource(res) => FileSystemResource.Of(BinaryResourceWrap.unwrap(res))
          case scalaApi.options.OutputValue.DirectoryResource(res) => DirectoryResourceWrap.unwrap(res)
        }
    }
}
