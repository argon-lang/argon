package dev.argon.backend.js

import dev.argon.compiler.{Compilation, RComp}
import dev.argon.compiler.core.{ArModule, ErasedSignature, GlobalBinding, GlobalId}
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.lookup.LookupNames
import dev.argon.compiler.vtable.VTableBuilder
import shapeless.Id
import zio.{Ref, Tag, ZIO}
import zio.stream.Stream
import cats.implicits._
import dev.argon.io.fileio.FileIO

abstract class JSEmitterModule extends JSEmitterGlobals {

  import JSDSL._

  val inject: JSInjectCode

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): RComp[FileIO, JSModule] =
    ZIO.accessM[FileIO] { env =>
      val file = env.get
      val modulePairs = module.referencedModules
        .zipWithIndex
        .map { case (refModule, i) => (refModule, id"module_${i.toString}") }


      val moduleEmit: Emit[JSModule] =
        for {
          injectBefore <- ZIO.foreach(inject.before) { singleFile =>
            file.readAllText(singleFile.file)
          }.catchAll(Compilation.unwrapThrowable)

          injectAfter <- ZIO.foreach(inject.after) { singleFile =>
            file.readAllText(singleFile.file)
          }.catchAll(Compilation.unwrapThrowable)

          vtableBuilder <- VTableBuilder(context)
          topLevelStmts <- module.bindings.foldM(ArModuleElements(Vector.empty, Vector.empty, Vector.empty, Vector.empty))(createObjectsForScopeValue(vtableBuilder))
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


  private def createObjectsForScopeValue(vtableBuilder: VTableBuilder.Aux[context.type])(state: ArModuleElements, value: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): Emit[ArModuleElements] =
    value match {
      case GlobalBinding.GlobalFunction(_, _, _, funcComp) =>
        for {
          func <- funcComp
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

      case GlobalBinding.GlobalTrait(_, _, _, arTraitComp) =>
        for {
          arTrait <- arTraitComp
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

      case GlobalBinding.GlobalClass(_, _, _, arClassComp) =>
        for {
          arClass <- arClassComp
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

      case GlobalBinding.GlobalDataConstructor(_, _, _, ctorComp) =>
        for {
          ctor <- ctorComp
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
              ctorCreator: _*
            )
          )
        } yield state.copy(dataConstructors = state.dataConstructors :+ ctorInfo)
    }


}
