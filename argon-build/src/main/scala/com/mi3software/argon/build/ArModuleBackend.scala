package com.mi3software.argon.build
import java.io.OutputStream
import java.util.zip.ZipOutputStream

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.module._
import com.mi3software.argon.util.stream.ArStream
import scalapb.GeneratedMessage
import scalaz._
import Scalaz._
import com.mi3software.argon.util.FileOperations
import scalaz.Leibniz.===
import scalaz.zio.{IO, ZIO}

object ArModuleBackend extends Backend {
  override type TCompilationOutput[F[+_]] = CompilationOutput[F]

  override val id: String = "argon-module"
  override val name: String = "Argon Module"


  override def compile[F[+ _], I: Show](input: CompilerInput[I])(implicit comp: Compilation[F], res: ResourceAccess[F, I]): F[CompilationOutput[F]] = {
    val context = new ModuleContext[F]
    val emitter = new ModuleEmitter[F, context.type](context)

    context.createModule(input)
      .map(emitter.emitModule _ andThen createOutput[F])
  }

  private def createOutput[F[+_]: Monad](moduleStream: ArStream[F, (String, GeneratedMessage), Unit]): CompilationOutput[F] = new CompilationOutput[F] {
    override def write[F2[_, _] : MonadErrorThrowable : ZIO](stream: OutputStream)(implicit ev: F[Unit] === F2[Throwable, Unit]): F2[Throwable, Unit] =
      FileOperations.zipOutputStream(stream) { zip =>
        ev(moduleStream.foldLeftM(()) {
          case (_, _) => ()
        } {
          case (_, (path, message)) =>
            ev.flip(FileOperations.createZipEntry(zip, path) { entry =>
              ZIO[F2].liftZIO(IO.syncThrowable { message.writeTo(zip) })
            })
        })
      }


  }

}
