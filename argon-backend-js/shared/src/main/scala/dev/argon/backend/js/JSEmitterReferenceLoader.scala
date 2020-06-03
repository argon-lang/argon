package dev.argon.backend.js

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler.core._
import dev.argon.compiler.lookup.LookupNames
import dev.argon.util.NamespacePath
import zio._
import cats.implicits._

private[js] trait JSEmitterReferenceLoader extends JSEmitterBase {

  import JSDSL._

  def coreLibExport(exportName: String): Emit[JSExpression] =
    ZIO.accessM[EmitEnv] { emitEnv =>
      if(emitEnv.module.id.name === LookupNames.argonCoreLib)
        IO.succeed(id"$exportName")
      else
        IO.fromEither(
          emitEnv.modulePairs
            .find { case (module, _) => module.id.name === LookupNames.argonCoreLib }
            .map { case (_, importName) => importName.cprop(LookupNames.argonCoreLib).prop(id"$exportName") }
            .toRight { NonEmptyList.of(CompilationError.ReferencedModuleNotFound(dev.argon.module.ModuleReference(LookupNames.argonCoreLib), CompilationMessageSource.EmitPhase())) }
        )

    }

  def getModuleJSObject(moduleId: ModuleId): Emit[JSExpression] =
    ZIO.accessM[EmitEnv] { emitEnv =>
      if(emitEnv.module.id === moduleId)
        IO.succeed(currentModuleVarName)
      else
        IO.fromOption(
          emitEnv.modulePairs
            .find { case (module, _) => module.id === moduleId }
            .map { case (_, importName) => importName }
        )
          .mapError[ErrorList] { _ =>
            NonEmptyList.of(CompilationError.ReferencedModuleNotFound(dev.argon.module.ModuleReference(LookupNames.argonCoreLib), CompilationMessageSource.EmitPhase()))
          }

    }

  def convertNamespacePath(ns: NamespacePath): JSExpression =
    JSArrayLiteral(ns.ns.map(JSString.apply))

  def convertGlobalName(name: GlobalName, id: GlobalId): Emit[JSExpression] =
    name match {
      case GlobalName.Normal(name) => IO.succeed(id"$name")
      case GlobalName.Unnamed => ZIO.accessM(_.unnamedSymbols.modify { symbols =>
        symbols.get(id) match {
          case Some(symbolIdent) => (symbolIdent, symbols)
          case None =>
            val symbolIdent = id"unnamed_${symbols.size}"
            (symbolIdent, symbols + (id -> symbolIdent))
        }
      })
    }

  def convertSignature(sig: ErasedSignature[context.type]): JSExpression = ???
  def convertParameterOnlySignature(sig: ErasedSignature.ParameterOnlySignature[context.type]): JSExpression = ???

  def getClassJSObject[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec], sig: ErasedSignature.ParameterOnlySignature[context.type]): Emit[JSExpression] =
    arClass.owner match {
      case ClassOwner.ByNamespace(module, namespace, name) =>
        for {
          moduleObj <- getModuleJSObject(module.id)
          nameValue <- convertGlobalName(name, arClass.id)
        } yield moduleObj.prop(id"globalClass")(convertNamespacePath(namespace), nameValue, convertParameterOnlySignature(sig))
    }

  def getTraitJSObject[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec], sig: ErasedSignature.ParameterOnlySignature[context.type]): Emit[JSExpression] =
    arTrait.owner match {
      case TraitOwner.ByNamespace(module, namespace, name) =>
        for {
          moduleObj <- getModuleJSObject(module.id)
          nameValue <- convertGlobalName(name, arTrait.id)
        } yield moduleObj.prop(id"globalTrait")(convertNamespacePath(namespace), nameValue, convertParameterOnlySignature(sig))
    }


  def getDataCtorJSObject[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec], sig: ErasedSignature.ParameterOnlySignature[context.type]): Emit[JSExpression] =
    ctor.owner match {
      case DataConstructorOwner.ByNamespace(module, namespace, name) =>
        for {
          moduleObj <- getModuleJSObject(module.id)
          nameValue <- convertGlobalName(name, ctor.id)
        } yield moduleObj.prop(id"globalDataConstructor")(convertNamespacePath(namespace), nameValue, convertParameterOnlySignature(sig))
    }

  def getMethodLookupFunction[TPayloadSpec[_, _]](methodOwner: MethodOwner[context.type, TPayloadSpec]): Emit[JSExpression] =
    methodOwner match {
      case MethodOwner.ByTrait(ownerTrait) =>
        ownerTrait.signature
          .flatMap { sig => getTraitJSObject(ownerTrait, ErasedSignature.fromSignatureParameters(context)(sig)) }
          .map { _.prop(id"method") }

      case MethodOwner.ByTraitObject(ownerTrait) =>
        ownerTrait.signature
          .flatMap { sig => getTraitJSObject(ownerTrait, ErasedSignature.fromSignatureParameters(context)(sig)) }
          .map { _.prop(id"staticMethod") }

      case MethodOwner.ByClass(ownerClass) =>
        ownerClass.signature
          .flatMap { sig => getClassJSObject(ownerClass, ErasedSignature.fromSignatureParameters(context)(sig)) }
          .map { _.prop(id"method") }
      case MethodOwner.ByClassObject(ownerClass) =>
        ownerClass.signature
          .flatMap { sig => getClassJSObject(ownerClass, ErasedSignature.fromSignatureParameters(context)(sig)) }
          .map { _.prop(id"staticMethod") }

      case MethodOwner.ByDataCtor(dataCtor) =>
        dataCtor.signature
          .flatMap { sig => getDataCtorJSObject(dataCtor, ErasedSignature.fromSignatureParameters(context)(sig)) }
          .map { _.prop(id"method") }
    }

  def getMethodName(methodName: MethodName): JSExpression =
    methodName match {
      case MemberName.Normal(name) => JSString(name)
      case MemberName.Mutator(name) => ???
      case MemberName.Unnamed => ???
      case MemberName.Call => ???
    }

  def getMethodJSObject[TPayloadSpec[_, _]](method: ArMethod[context.type, TPayloadSpec]): Emit[JSExpression] = for {
    sig <- method.signatureUnsubstituted
    lookupFunction <- getMethodLookupFunction(method.owner)
    name = getMethodName(method.name)
  } yield lookupFunction(name, convertSignature(ErasedSignature.fromSignature(context)(sig)))

  def getClassConstructorJSObject[TPayloadSpec[_, _]](ctor: ClassConstructor[context.type, TPayloadSpec]): Emit[JSExpression] = for {
    classSig <- ctor.ownerClass.signature
    ownerClassObj <- getClassJSObject(ctor.ownerClass, ErasedSignature.fromSignatureParameters(context)(classSig))
    sig <- ctor.signatureUnsubstituted
  } yield ownerClassObj.prop(id"constructor")(convertParameterOnlySignature(ErasedSignature.fromSignatureParameters(context)(sig)))

  def getFunctionJSObject[TPayloadSpec[_, _]](func: ArFunc[context.type, TPayloadSpec]): Emit[JSExpression] = for {
    sig <- func.signature
    result <- func.owner match {
      case FunctionOwner.ByNamespace(module, namespace, name) =>
        val jsSig = convertSignature(ErasedSignature.fromSignature(context)(sig))

        for {
          moduleObj <- getModuleJSObject(module.id)
          nameValue <- convertGlobalName(name, func.id)
        } yield moduleObj.prop(id"globalFunction")(convertNamespacePath(namespace), nameValue, jsSig)
    }
  } yield result
}
