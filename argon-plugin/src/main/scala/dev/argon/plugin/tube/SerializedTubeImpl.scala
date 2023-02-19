package dev.argon.plugin.tube

import dev.argon.compiler.Context
import dev.argon.compiler.module.ModulePath as CompilerModulePath
import dev.argon.io.{BinaryResource, DirectoryResource, FileSystemResource, ResourceRecorder}
import dev.argon.tube.*
import zio.*
import zio.stm.TMap
import dev.argon.util.{*, given}
import java.util.NoSuchElementException

private[tube] object SerializedTubeImpl {
  def apply[R, E](writer: TubeWriterBase { val context: Context { type Env = R; type Error = E } }): ZIO[R, E, SerializedTube[R, E]] =
    for
      _ <- writer.scanTubeRefs
      _ <- writer.scanTubeDefs

      resources <- TMap.empty[String, FileSystemResource[R, E]].commit

      metadataMemo <- writer.emitMetadata().memoize
    yield new SerializedTube[R, E] {
      override def version: ZIO[R, E, TubeFormatVersion] =
        ZIO.succeed(TubeProto.defaultTubeFormatVersion.get(TubeFormatVersion.scalaDescriptor.getOptions).get)

      override def metadata: ZIO[R, E, Metadata] =
        metadataMemo

      override def getResource(id: String): ZIO[R, E, FileSystemResource[R, E]] =
        resources.get(id).commit.flatMap {
          case Some(value) => ZIO.succeed(value)
          case None => ZIO.die(NoSuchElementException())
        }

      override def getModule(modulePath: ModulePath): ZIO[R, E, ModuleDefinition] =
        writer.emitModule(CompilerModulePath(modulePath.name))

      override def getClass(id: BigInt): ZIO[R, E, ClassDefinition] =
        writer.emitClass(id)

      override def getTrait(id: BigInt): ZIO[R, E, TraitDefinition] =
        writer.emitTrait(id)

      override def getFunction(id: BigInt): ZIO[R, E, FunctionDefinition] =
        writer.emitFunction(id)

      override def getMethod(id: BigInt): ZIO[R, E, MethodDefinition] =
        writer.emitMethod(id)

      override def getClassConstructor(id: BigInt): ZIO[R, E, ClassConstructorDefinition] =
        writer.emitClassConstructor(id)
    }


}
