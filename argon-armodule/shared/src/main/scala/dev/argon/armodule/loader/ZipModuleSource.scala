package dev.argon.armodule.loader

import dev.argon.armodule._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.output.ArgonModuleSerialized
import dev.argon.io.ZipFileReader
import dev.argon.module._
import dev.argon.util.{MaybeBlocking, ProtoBufCodecs, StreamableMessage}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream._

object ZipModuleSource {

  def tryOpen(zip: ZipFileReader[CompilationError]): RComp[MaybeBlocking, Option[ArgonModuleSerialized]] = {
    ZIO.environment[MaybeBlocking].flatMap { protoBufEnv =>
      def tryReadZipMessage[A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(path: String): Comp[Option[A]] =
        zip.getEntryStream(path).flatMap {
          case Some(stream) => ProtoBufCodecs.deserializeProtocolBuffer(companion)(stream).provide(protoBufEnv)(zio.NeedsEnv).catchAll(Compilation.unwrapThrowable).asSome
          case None => IO.none
        }

      tryReadZipMessage(Metadata)(ModulePaths.metadata).map { metadataOpt =>
        metadataOpt.map { metadata2 =>
          new ArgonModuleSerialized {

            private def readZipMessage[A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(path: String): Comp[A] =
              tryReadZipMessage(companion)(path).flatMap {
                case Some(a) => IO.succeed(a)
                case None => Compilation.forErrors(DiagnosticError.ModuleFormatInvalid(DiagnosticSource.ReferencedModule(ModuleId(metadata.name))))
              }

            private def readZipStream[A >: Null <: AnyRef](companion: StreamableMessage[A])(path: String): CompStream[A] =
              ZStream.unwrap(zip.getEntryStream(path).map {
                case Some(stream) => ProtoBufCodecs.deserializeProtocolBufferStream(companion)(stream).provide(protoBufEnv)(zio.NeedsEnv).catchAll(Compilation.unwrapThrowableStream)
                case None => Stream.fail(DiagnosticError.ModuleFormatInvalid(DiagnosticSource.ReferencedModule(ModuleId(metadata.name))))
              })

            private def readZipPayload(path: String): CompStream[Byte] =
              ZStream.unwrap(zip.getEntryStream(path).map {
                case Some(stream) => stream.catchAll(Compilation.unwrapThrowableStream)
                case None => Stream.fail(DiagnosticError.ModuleFormatInvalid(DiagnosticSource.ReferencedModule(ModuleId(metadata.name))))
              })

            override val metadata: Metadata = metadata2

            override def references: Comp[ModuleReferencesList] =
              readZipMessage(ModuleReferencesList)(ModulePaths.referencedModules)

            override def namespaces: CompStream[NamespaceDeclaration] =
              readZipStream(NamespaceDeclarationCompanion)(ModulePaths.namespaceIndex)

            override def namespaceElements(id: Int): CompStream[GlobalDeclarationElement] =
              readZipStream(GlobalDeclarationElementCompanion)(ModulePaths.namespaceContent(id))

            override def getTraitDef(id: Int): Comp[TraitDefinition] =
              readZipMessage(TraitDefinition)(ModulePaths.elementDef(ModulePaths.traitTypeName, id))

            override def getTraitRef(id: Int): Comp[TraitReference] =
              readZipMessage(TraitReference)(ModulePaths.elementRef(ModulePaths.traitTypeName, id))

            override def getClassDef(id: Int): Comp[ClassDefinition] =
              readZipMessage(ClassDefinition)(ModulePaths.elementDef(ModulePaths.classTypeName, id))

            override def getClassRef(id: Int): Comp[ClassReference] =
              readZipMessage(ClassReference)(ModulePaths.elementDef(ModulePaths.classTypeName, id))

            override def getDataConstructorDef(id: Int): Comp[DataConstructorDefinition] =
              readZipMessage(DataConstructorDefinition)(ModulePaths.elementDef(ModulePaths.dataCtorTypeName, id))

            override def getDataConstructorRef(id: Int): Comp[DataConstructorReference] =
              readZipMessage(DataConstructorReference)(ModulePaths.elementDef(ModulePaths.dataCtorTypeName, id))

            override def getFunctionDef(id: Int): Comp[FunctionDefinition] =
              readZipMessage(FunctionDefinition)(ModulePaths.elementDef(ModulePaths.funcTypeName, id))

            override def getFunctionRef(id: Int): Comp[FunctionReference] =
              readZipMessage(FunctionReference)(ModulePaths.elementDef(ModulePaths.funcTypeName, id))

            override def getFunctionPayload(id: Int, platformId: String): CompStream[Byte] =
              readZipPayload(ModulePaths.extern(platformId, ModulePaths.funcTypeName, id))

            override def getMethodDef(id: Int): Comp[MethodDefinition] =
              readZipMessage(MethodDefinition)(ModulePaths.elementDef(ModulePaths.methodTypeName, id))

            override def getMethodRef(id: Int): Comp[MethodReference] =
              readZipMessage(MethodReference)(ModulePaths.elementDef(ModulePaths.methodTypeName, id))

            override def getMethodPayload(id: Int, platformId: String): CompStream[Byte] =
              readZipPayload(ModulePaths.extern(platformId, ModulePaths.methodTypeName, id))

            override def getClassConstructorDef(id: Int): Comp[ClassConstructorDefinition] =
              readZipMessage(ClassConstructorDefinition)(ModulePaths.elementDef(ModulePaths.classCtorTypeName, id))

            override def getClassConstructorRef(id: Int): Comp[ClassConstructorReference] =
              readZipMessage(ClassConstructorReference)(ModulePaths.elementDef(ModulePaths.classCtorTypeName, id))

            override def getClassConstructorPayload(id: Int, platformId: String): CompStream[Byte] =
              readZipPayload(ModulePaths.extern(platformId, ModulePaths.methodTypeName, id))
          }
        }
      }
    }
  }

}

