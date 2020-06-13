package dev.argon.backend.js

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler.core._
import dev.argon.compiler.lookup.LookupNames
import dev.argon.util.NamespacePath
import zio._
import zio.interop.catz.core._
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
            .map { case (_, JSIdentifier(importName)) => id"${importName}_all".prop(id"$exportName") }
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
      case GlobalName.Normal(name) => IO.succeed(JSString(name))
      case GlobalName.Operator(op) => IO.succeed(jsobj("type" -> JSString("operator"), "name" -> JSString(op)))
      case GlobalName.Unnamed => ZIO.accessM(_.unnamedSymbols.modify { symbols =>
        symbols.get(id) match {
          case Some(symbolIdent) => (symbolIdent, symbols)
          case None =>
            val symbolIdent = id"unnamed_${symbols.size}"
            (symbolIdent, symbols + (id -> symbolIdent))
        }
      })
    }

  def convertSignatureType(t: ErasedSignature.SigType[context.type]): Emit[JSExpression] =
    t match {
      case ErasedSignature.BlankType() => IO.succeed(jsobj("type" -> JSString("blank")))
      case ErasedSignature.TraitType(arTrait, typeArgs) =>
        for {
          sig <- arTrait.value.signature
          traitObj <- getTraitJSObject(arTrait.value, ErasedSignature.fromSignatureParameters(context)(sig))
          args <- typeArgs.traverse(convertSignatureType)
        } yield jsobj(
          "type" -> JSString("trait"),
          "arTrait" -> traitObj,
          "arguments" -> JSArrayLiteral(args),
        )

      case ErasedSignature.ClassType(arClass, typeArgs) =>
        for {
          sig <- arClass.value.signature
          classObj <- getClassJSObject(arClass.value, ErasedSignature.fromSignatureParameters(context)(sig))
          args <- typeArgs.traverse(convertSignatureType)
        } yield jsobj(
          "type" -> JSString("class"),
          "arClass" -> classObj,
          "arguments" -> JSArrayLiteral(args),
        )

      case ErasedSignature.DataConstructorType(ctor, typeArgs) =>
        for {
          sig <- ctor.value.signature
          ctorObj <- getDataCtorJSObject(ctor.value, ErasedSignature.fromSignatureParameters(context)(sig))
          args <- typeArgs.traverse(convertSignatureType)
        } yield jsobj(
          "type" -> JSString("data-constructor"),
          "dataConstructor" -> ctorObj,
          "arguments" -> JSArrayLiteral(args),
        )

      case ErasedSignature.TupleType(elements) =>
        elements.toList.toVector
          .traverse(convertSignatureType)
          .map { elemTypes =>
            jsobj(
              "type" -> JSString("tuple"),
              "elements" -> JSArrayLiteral(elemTypes),
            )
          }

      case ErasedSignature.FunctionType(argumentType, resultType) =>
        for {
          argType <- convertSignatureType(argumentType)
          resType <- convertSignatureType(resultType)
        } yield jsobj(
          "type" -> JSString("function"),
          "argumentType" -> argType,
          "resultType" -> resType,
        )
    }

  def convertSignature(sig: ErasedSignature[context.type]): Emit[JSExpression] = {
    def impl(sig: ErasedSignature[context.type], acc: Vector[JSExpression]): Emit[JSExpression] =
      sig match {
        case ErasedSignature.Parameter(paramType, next) =>
          convertSignatureType(paramType).flatMap { convParamType =>
            impl(next, acc :+ convParamType)
          }

        case ErasedSignature.Result(resultType) =>
          convertSignatureType(resultType).map { convResultType =>
            jsobj(
              "parameterTypes" -> JSArrayLiteral(acc),
              "resultType" -> convResultType
            )
          }
      }

    impl(sig, Vector.empty)
  }

  def convertParameterOnlySignature(sig: ErasedSignature.ParameterOnlySignature[context.type]): Emit[JSExpression] =
    for {
      paramTypes <- sig.paramTypes.traverse(convertSignatureType)
    } yield jsobj(
      "parameterTypes" -> JSArrayLiteral(paramTypes)
    )

  def getClassJSObject[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec], sig: ErasedSignature.ParameterOnlySignature[context.type]): Emit[JSExpression] =
    arClass.owner match {
      case ClassOwner.ByNamespace(module, namespace, name) =>
        for {
          moduleObj <- getModuleJSObject(module.id)
          nameValue <- convertGlobalName(name, arClass.id)
          convSig <- convertParameterOnlySignature(sig)
        } yield moduleObj.prop(id"globalClass")(convertNamespacePath(namespace), nameValue, convSig)
    }

  def getTraitJSObject[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec], sig: ErasedSignature.ParameterOnlySignature[context.type]): Emit[JSExpression] =
    arTrait.owner match {
      case TraitOwner.ByNamespace(module, namespace, name) =>
        for {
          moduleObj <- getModuleJSObject(module.id)
          nameValue <- convertGlobalName(name, arTrait.id)
          convSig <- convertParameterOnlySignature(sig)
        } yield moduleObj.prop(id"globalTrait")(convertNamespacePath(namespace), nameValue, convSig)
    }


  def getDataCtorJSObject[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec], sig: ErasedSignature.ParameterOnlySignature[context.type]): Emit[JSExpression] =
    ctor.owner match {
      case DataConstructorOwner.ByNamespace(module, namespace, name) =>
        for {
          moduleObj <- getModuleJSObject(module.id)
          nameValue <- convertGlobalName(name, ctor.id)
          convSig <- convertParameterOnlySignature(sig)
        } yield moduleObj.prop(id"globalDataConstructor")(convertNamespacePath(namespace), nameValue, convSig)
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

  def getMethodName(methodName: MethodName): Emit[JSExpression] =
    methodName match {
      case MemberName.Normal(name) => IO.succeed(JSString(name))
      case MemberName.Mutator(name) => IO.succeed(jsobj("type" -> JSString("mutator"), "name" -> JSString(name)))
      case MemberName.Unnamed => IO.succeed(JSNull)
      case MemberName.Call => coreLibExport("SpecialMethodName").map { _.prop(id"Call") }
    }

  def getMethodJSObject[TPayloadSpec[_, _]](method: ArMethod[context.type, TPayloadSpec]): Emit[JSExpression] = for {
    sig <- method.signatureUnsubstituted
    lookupFunction <- getMethodLookupFunction(method.owner)
    name <- getMethodName(method.name)
    convSig <- convertSignature(ErasedSignature.fromSignature(context)(sig))
  } yield lookupFunction(name, convSig)

  def getClassConstructorJSObject[TPayloadSpec[_, _]](ctor: ClassConstructor[context.type, TPayloadSpec]): Emit[JSExpression] = for {
    classSig <- ctor.ownerClass.signature
    ownerClassObj <- getClassJSObject(ctor.ownerClass, ErasedSignature.fromSignatureParameters(context)(classSig))
    sig <- ctor.signatureUnsubstituted
    convSig <- convertParameterOnlySignature(ErasedSignature.fromSignatureParameters(context)(sig))
  } yield ownerClassObj.prop(id"constructor")(convSig)

  def getFunctionJSObject[TPayloadSpec[_, _]](func: ArFunc[context.type, TPayloadSpec]): Emit[JSExpression] = for {
    sig <- func.signature
    result <- func.owner match {
      case FunctionOwner.ByNamespace(module, namespace, name) =>
        for {
          jsSig <- convertSignature(ErasedSignature.fromSignature(context)(sig))
          moduleObj <- getModuleJSObject(module.id)
          nameValue <- convertGlobalName(name, func.id)
        } yield moduleObj.prop(id"globalFunction")(convertNamespacePath(namespace), nameValue, jsSig)
    }
  } yield result
}
