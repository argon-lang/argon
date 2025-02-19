package dev.argon.backend

import dev.argon.backend.scalaApi.ScopedResource
import dev.argon.compiler.TubeName
import dev.argon.util.async.ErrorWrapper
import dev.argon.vm
import dev.argon.vm.resource.VmIrResource
import zio.*

import java.io.IOException

object ScalaApiBackendLoader {
  def loadScalaApiBackend[E >: BackendException | IOException, Opts, Outs](using ew: ErrorWrapper[E])(backendName: String)(backend: scalaApi.Backend[ew.EX, Opts, Outs]): UIO[Backend[E]] =
    for
      apiCodeGen <- backend.codeGenerator()
    yield new Backend[E] {
      override type Output = Outs

      override def name: String = backendName

      override val codeGenerator: CodeGenerator[E, Outs] =
        apiCodeGen match {
          case scalaApi.CodeGenerator.Library(libCodeGen) =>
            new CodeGenerator.LibraryCodeGenerator[E, Outs] {
              override type Options = Opts

              override def codegen(options: Opts, program: VmIrResource[E], libraries: Map[TubeName, VmIrResource[E]]): ZIO[Scope, E, Outs] =
                libCodeGen.codegen(
                  options,
                  vmIrToApi(program),
                  scalaApi.LibraryMap(
                    libraries
                      .view
                      .map { (name, res) =>
                        scalaApi.LibraryMapEntry(
                          vm.TubeName(name.parts.head, name.parts.tail),
                          vmIrToApi(res)
                        )
                      }
                      .toSeq
                  )
                )
                  .catchAll(e => ZIO.failCause(ew.unwrap(e)))
            }
        }

      private def vmIrToApi(res: VmIrResource[E]): scalaApi.VmIrTube[ew.EX] =
        new scalaApi.VmIrTube[ew.EX] {
          override def stream(): IO[ew.EX, ScopedResource[scalaApi.Stream[ew.EX, vm.TubeFileEntry]]] =
            scopedResource(
              for
                pull <- ErrorWrapper.wrapStream(res.decoded).toPull
              yield new scalaApi.Stream[ew.EX, vm.TubeFileEntry] {
                override def next(): IO[ew.EX, Seq[vm.TubeFileEntry]] =
                  pull.foldZIO(
                    failure = {
                      case Some(e) => ZIO.fail(e)
                      case None => ZIO.succeed(Seq.empty)
                    },
                    success = chunk => {
                      if chunk.isEmpty then
                        next()
                      else
                        ZIO.succeed(chunk)
                    },
                  )
              }
            )
        }

      private def scopedResource[E, A](io: ZIO[Scope, E, A]): IO[E, ScopedResource[A]] =
        Scope.make.flatMap { scope =>
          io.provideEnvironment(ZEnvironment(scope))
            .foldCauseZIO(
              failure = cause => scope.close(Exit.failCause(cause)) *> ZIO.failCause(cause),
              success = a => ZIO.succeed(new ScopedResource[A] {
                override def get(): UIO[A] = ZIO.succeed(a)
                override def close(): UIO[Unit] = scope.close(Exit.unit)
              })
            )
        }
    }
}
