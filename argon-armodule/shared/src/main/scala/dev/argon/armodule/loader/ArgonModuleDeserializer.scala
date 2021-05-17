package dev.argon.armodule.loader

import dev.argon.armodule.loader.ModuleCreatorCommon._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.UnlinkedModule
import dev.argon.compiler.output.ArgonModuleSerialized
import dev.argon.module.{ModuleFormatVersion, ModuleReferencesList}
import dev.argon.util.{MemoCacheStore, ValueCacheManaged}
import dev.argon.{module => ArgonModule}
import zio.{Managed, ZManaged}

import scala.collection.immutable.Vector

object ArgonModuleDeserializer {

  def deserialize[TContext <: Context](serialized: ArgonModuleSerialized): Comp[UnlinkedModule[TContext, ReferencePayloadSpecifier]] =
    serialized.references.map { references =>
      new ArUnlinkedModule[TContext](serialized, references)
    }


  private def createReferencePayloadLoader[TContext <: Context]: PayloadLoader[TContext, ReferencePayloadSpecifier] =
    new PayloadLoader[TContext, ReferencePayloadSpecifier] {
      override def createDataConstructorPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TDataConstructorImplementation], Unit] = ()

      override def createFunctionPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TFunctionImplementation], Unit] = ()

      override def createMethodPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TMethodImplementation], Unit] = ()

      override def createClassConstructorPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TClassConstructorImplementation], Unit] = ()
    }

  private final class ArUnlinkedModule[TContext <: Context](source: ArgonModuleSerialized, references: ArgonModule.ModuleReferencesList) extends UnlinkedModule[TContext, ReferencePayloadSpecifier] {

    override val descriptor: ModuleId = ModuleId(source.metadata.name)
    override val referencedModules: Vector[ModuleId] =
      references.references.map {
        case ArgonModule.ModuleReference(name) => ModuleId(name)
      }

    override def load(context: TContext)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): CompManaged[ArModule[context.type, ReferencePayloadSpecifier]] =
      loadModule[ReferencePayloadSpecifier](context)(source, references)(referencedModules)(createReferencePayloadLoader[TContext])


    private def loadModule[TPayloadSpec[_, _]: PayloadSpecInfo]
    (context: TContext)
    (source: ArgonModuleSerialized, referenceList: ArgonModule.ModuleReferencesList)
    (referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]])
    (payloadLoader: PayloadLoader[TContext, TPayloadSpec])
    : Managed[CompilationError, ArModule[context.type, TPayloadSpec]] = {

      import source.metadata

      val context2: context.type = context
      import context.{typeSystem, signatureContext}, typeSystem.{ context => _ }, signatureContext.{ context => _, TTypeWrapper => _, typeWrapperInstances => _ }

      val source2 = source
      val referenceList2 = referenceList
      val referencedModules2 = referencedModules
      val payloadLoader2 = payloadLoader

      val currentModuleId = ModuleId(metadata.name)

      for {

        _ <- ZManaged.fromEffect(Compilation.require(
          metadata.formatVersion > 0 && metadata.formatVersion <= ModuleFormatVersion.currentVersion
        )(
          DiagnosticError.UnsupportedModuleFormatVersion(metadata.formatVersion, DiagnosticSource.ReferencedModule(currentModuleId))
        ))

        moduleCache2 <- ValueCacheManaged.make[CompilationError, ArModule[context.type, TPayloadSpec]]
        traitCache2 <- ZManaged.fromEffect(MemoCacheStore.make[CompilationError, Int, TraitLoadResult[context.type, TPayloadSpec]])
        classCache2 <- ZManaged.fromEffect(MemoCacheStore.make[CompilationError, Int, ClassLoadResult[context.type, TPayloadSpec]])
        dataCtorCache2 <- ZManaged.fromEffect(MemoCacheStore.make[CompilationError, Int, DataCtorLoadResult[context.type, TPayloadSpec]])
        functionCache2 <- ZManaged.fromEffect(MemoCacheStore.make[CompilationError, Int, FunctionLoadResult[context.type, TPayloadSpec]])
        methodCache2 <- ZManaged.fromEffect(MemoCacheStore.make[CompilationError, Int, MethodLoadResult[context.type, TPayloadSpec]])
        classCtorCache2 <- ZManaged.fromEffect(MemoCacheStore.make[CompilationError, Int, ClassCtorLoadResult[context.type, TPayloadSpec]])
        localVariableIdCache2 <- ZManaged.fromEffect(MemoCacheStore.make[CompilationError, Int, LocalVariableId])

        module <- ZManaged.fromEffect(new ModuleCreatorCommon[TPayloadSpec] {
          override val context: context2.type = context2
          override val source: ArgonModuleSerialized = source2
          override val payloadLoader: PayloadLoader[_ >: context.type, TPayloadSpec] = payloadLoader2

          override val currentModuleDescriptor: ModuleId = currentModuleId
          override val referenceList: ModuleReferencesList = referenceList2
          override val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]] = referencedModules2
          override val moduleCache: ValueCacheManaged[CompilationError, ArModule[context.type, TPayloadSpec]] = moduleCache2
          override val traitCache: MemoCacheStore[CompilationError, Int, TraitLoadResult[context.type, TPayloadSpec]] = traitCache2
          override val classCache: MemoCacheStore[CompilationError, Int, ClassLoadResult[context.type, TPayloadSpec]] = classCache2
          override val dataCtorCache: MemoCacheStore[CompilationError, Int, DataCtorLoadResult[context.type, TPayloadSpec]] = dataCtorCache2
          override val functionCache: MemoCacheStore[CompilationError, Int, FunctionLoadResult[context.type, TPayloadSpec]] = functionCache2
          override val methodCache: MemoCacheStore[CompilationError, Int, MethodLoadResult[context.type, TPayloadSpec]] = methodCache2
          override val classCtorCache: MemoCacheStore[CompilationError, Int, ClassCtorLoadResult[context.type, TPayloadSpec]] = classCtorCache2
          override val localVariableIdCache: MemoCacheStore[CompilationError, Int, LocalVariableId] = localVariableIdCache2
        }.module)

      } yield module
    }

  }

}
