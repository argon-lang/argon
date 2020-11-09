package dev.argon.armodule.loader

import dev.argon.armodule._
import dev.argon.compiler.loaders.ResourceReader
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.io.{StreamableMessage, ZipFileReader}
import dev.argon.io.fileio.FileIOLite
import dev.argon.module._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream._

object ZipModuleSource {

  def tryOpen(zip: ZipFileReader[Any, CompilationError], res: ResourceReader.Service[_]): Comp[Option[ArgonModuleSource]] = {
    def tryReadZipMessage[A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(path: String): Comp[Option[A]] =
      zip.getEntryStream(path).flatMap {
        case Some(stream) => res.deserializeProtocolBuffer(companion)(stream).asSome
        case None => IO.none
      }

    tryReadZipMessage(Metadata)(ModulePaths.metadata).map { metadataOpt =>
      metadataOpt.map { metadata2 =>
        new ArgonModuleSource {

          private def readZipMessage[A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(path: String): Comp[A] =
            tryReadZipMessage(companion)(path).flatMap {
              case Some(a) => IO.succeed(a)
              case None => Compilation.forErrors(DiagnosticError.ModuleFormatInvalid(DiagnosticSource.ReferencedModule(ModuleId(metadata.name))))
            }

          private def readZipStream[A >: Null <: AnyRef](companion: StreamableMessage[A])(path: String): CompStream[A] =
            ZStream.unwrap(zip.getEntryStream(path).map {
              case Some(stream) => res.deserializeProtocolBufferStream(companion)(stream)
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

          override def getMethodDef(id: Int): Comp[MethodDefinition] =
            readZipMessage(MethodDefinition)(ModulePaths.elementDef(ModulePaths.methodTypeName, id))

          override def getMethodRef(id: Int): Comp[MethodReference] =
            readZipMessage(MethodReference)(ModulePaths.elementDef(ModulePaths.methodTypeName, id))

          override def getClassConstructorDef(id: Int): Comp[ClassConstructorDefinition] =
            readZipMessage(ClassConstructorDefinition)(ModulePaths.elementDef(ModulePaths.classCtorTypeName, id))

          override def getClassConstructorRef(id: Int): Comp[ClassConstructorReference] =
            readZipMessage(ClassConstructorReference)(ModulePaths.elementDef(ModulePaths.classCtorTypeName, id))
        }
      }
    }
  }

}

