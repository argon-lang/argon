package dev.argon.backend.js

import dev.argon.compiler.RComp
import dev.argon.compiler.core.{ArModule, ErasedSignature, GlobalBinding, GlobalId, Namespace}
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.lookup.LookupNames
import dev.argon.compiler.vtable.VTableBuilder
import shapeless.Id
import zio.{Ref, Tag, ZIO}
import zio.stream.Stream
import cats.implicits._

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
              if(refModule.id.name === LookupNames.argonCoreLib)
                `import`(importId, `* as` = id"${importId.id}_all") from refModule.id.name
              else
                `import`(importId) from refModule.id.name
            },

            injectBefore.map(JSModuleRaw.apply).toList.toVector,

            Vector(
              const(currentModuleVarName ::= createModule(
                jsobj(
                  "globalClasses" -> JSArrayLiteral(topLevelStmts.classes),
                  "globalTraits" -> JSArrayLiteral(topLevelStmts.traits),
                  "globalDataConstructors" -> JSArrayLiteral(topLevelStmts.dataConstructors),
                  "globalFunctions" -> JSArrayLiteral(topLevelStmts.functions),
                )
              )),
              export default currentModuleVarName,
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
        for {
          funcCreator <- createGlobalFunction(func)

          globalName <- convertGlobalName(func.owner.name, func.id)
          sig <- func.signature
          jsSig <- convertSignature(ErasedSignature.fromSignature(context)(sig))

          funcInfo = jsobj(
            "type" -> JSString("global"),
            "ns" -> convertNamespacePath(func.owner.namespace),
            "name" -> globalName,
            get("sig")(
              JSReturn(jsSig),
            ),
            "create" -> jsfunction(None)(currentModuleVarName)(
              JSReturn(funcCreator),
            )
          )
        } yield state.copy(functions = state.functions :+ funcInfo)

      case GlobalBinding.GlobalTrait(_, _, arTrait) =>
        for {
          traitCreator <- createGlobalTrait(arTrait)

          globalName <- convertGlobalName(arTrait.owner.name, arTrait.id)
          sig <- arTrait.signature
          jsSig <- convertParameterOnlySignature(ErasedSignature.fromSignatureParameters(context)(sig))

          traitInfo = jsobj(
            "type" -> JSString("global"),
            "ns" -> convertNamespacePath(arTrait.owner.namespace),
            "name" -> globalName,
            get("sig")(
              JSReturn(jsSig),
            ),
            "create" -> jsfunction(None)(currentModuleVarName)(
              JSReturn(traitCreator),
            )
          )
        } yield state.copy(traits = state.traits :+ traitInfo)

      case GlobalBinding.GlobalClass(_, _, arClass) =>
        for {
          classCreator <- createGlobalClass(vtableBuilder)(arClass)

          globalName <- convertGlobalName(arClass.owner.name, arClass.id)
          sig <- arClass.signature
          jsSig <- convertParameterOnlySignature(ErasedSignature.fromSignatureParameters(context)(sig))

          classInfo = jsobj(
            "type" -> JSString("global"),
            "ns" -> convertNamespacePath(arClass.owner.namespace),
            "name" -> globalName,
            get("sig")(
              JSReturn(jsSig),
            ),
            "create" -> jsfunction(None)(currentModuleVarName)(
              JSReturn(classCreator),
            )
          )
        } yield state.copy(classes = state.classes :+ classInfo)

      case GlobalBinding.GlobalDataConstructor(_, _, ctor) =>
        for {
          ctorCreator <- createGlobalDataConstructor(vtableBuilder)(ctor)

          globalName <- convertGlobalName(ctor.owner.name, ctor.id)
          sig <- ctor.signature
          jsSig <- convertParameterOnlySignature(ErasedSignature.fromSignatureParameters(context)(sig))

          ctorInfo = jsobj(
            "type" -> JSString("global"),
            "ns" -> convertNamespacePath(ctor.owner.namespace),
            "name" -> globalName,
            get("sig")(
              JSReturn(jsSig),
            ),
            "create" -> jsfunction(None)(currentModuleVarName)(
              JSReturn(ctorCreator),
            )
          )
        } yield state.copy(dataConstructors = state.dataConstructors :+ ctorInfo)
    }


}
