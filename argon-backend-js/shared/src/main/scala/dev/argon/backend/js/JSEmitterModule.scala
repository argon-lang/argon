package dev.argon.backend.js

import dev.argon.compiler.RComp
import dev.argon.compiler.core.{ArModule, GlobalBinding, GlobalId, Namespace}
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.vtable.VTableBuilder
import shapeless.Id
import zio.{Ref, Tag, ZIO}
import zio.stream.Stream

abstract class JSEmitterModule[I <: ResourceIndicator: Tag] extends JSEmitterGlobals {

  import JSDSL._

  val inject: JSInjectCode[Id, I]

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): RComp[ResourceReader[I], JSModule] =
    ZIO.access[ResourceReader[I]](_.get).flatMap { resourceReader =>
      val modulePairs = module.referencedModules
        .zipWithIndex
        .map { case (refModule, i) => (refModule, id"module_${i.toString}") }


      val moduleEmit: Emit[JSModule] =
        for {
          injectBefore <- ZIO.foreach(inject.before) { singleFile =>
            resourceReader.readTextFileAsString(singleFile.file)
          }

          injectAfter <- ZIO.foreach(inject.after) { singleFile =>
            resourceReader.readTextFileAsString(singleFile.file)
          }

          globalNamespace <- module.globalNamespace
          vtableBuilder <- VTableBuilder(context)
          topLevelStmts <- allNamespaceElements(globalNamespace).foldM(ArModuleElements(Vector.empty, Vector.empty, Vector.empty, Vector.empty))(createObjectsForScopeValue(vtableBuilder))
          createModule <- coreLibExport("createModule")
        } yield JSModule(
          Vector(
            modulePairs.map { case (refModule, importId) =>
              import_* as importId from refModule.id.name
            },

            injectBefore.map(JSModuleRaw.apply).toList.toVector,

            Vector(
              export default createModule(
                jsobj(
                  "globalClasses" -> JSArrayLiteral(topLevelStmts.classes),
                  "globalTraits" -> JSArrayLiteral(topLevelStmts.traits),
                  "globalDataConstructors" -> JSArrayLiteral(topLevelStmts.dataConstructors),
                  "globalFunctions" -> JSArrayLiteral(topLevelStmts.functions),
                )
              )
            ),

            injectAfter.map(JSModuleRaw.apply).toList.toVector,

          ).flatten
        )

      for {
        unnamedSymbols <- Ref.make(Map.empty[GlobalId, JSIdentifier])
        nextSymbolId <- Ref.make(0)
        jsModule <- moduleEmit.provide(EmitEnv(
          module,
          modulePairs,
          unnamedSymbols,
          nextSymbolId,
          Map.empty,
        ))
      } yield jsModule
    }

  private def allNamespaceElements(namespace: Namespace[context.type, DeclarationPayloadSpecifier]): Stream[Nothing, GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] =
    Stream.fromIterable(namespace.bindings).flatMap {
      case GlobalBinding.NestedNamespace(_, ns) => allNamespaceElements(ns)
      case binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier] => Stream(binding)
    }


  private def createObjectsForScopeValue(vtableBuilder: VTableBuilder.Aux[context.type])(state: ArModuleElements, value: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): Emit[ArModuleElements] =
    value match {
      case GlobalBinding.GlobalFunction(_, _, func) =>
        createGlobalFunction(func).map { funcExpr => state.copy(functions = state.functions :+ funcExpr) }

      case GlobalBinding.GlobalTrait(_, _, arTrait) =>
        createGlobalTrait(arTrait).map { traitExpr => state.copy(traits = state.traits :+ traitExpr) }

      case GlobalBinding.GlobalClass(_, _, arClass) =>
        createGlobalClass(vtableBuilder)(arClass).map { classExpr => state.copy(classes = state.classes :+ classExpr) }

      case GlobalBinding.GlobalDataConstructor(_, _, ctor) =>
        createGlobalDataConstructor(vtableBuilder)(ctor).map { ctorExpr => state.copy(dataConstructors = state.dataConstructors :+ ctorExpr) }
    }


}
