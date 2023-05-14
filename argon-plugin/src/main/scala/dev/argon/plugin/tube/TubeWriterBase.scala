package dev.argon.plugin.tube

import com.google.protobuf.empty.Empty
import dev.argon.compiler.definitions.*
import dev.argon.compiler.module.{ModuleElementC, ModuleName, ModulePath}
import dev.argon.compiler.signature.*
import dev.argon.compiler.{definitions, *}
import dev.argon.io.*
import dev.argon.options.OptionCodec
import dev.argon.parser.{FunctionParameterListType, IdentifierExpr}
import dev.argon.tube as t
import dev.argon.util.toml.Toml
import dev.argon.util.{*, given}
import scalapb.{GeneratedEnum, GeneratedMessage}
import zio.*
import zio.stm.*
import zio.direct.*

import java.io.IOException
import java.util.NoSuchElementException

private[tube] abstract class TubeWriterBase extends UsingContext {


  val context: Context
  type IsImplementation <: Boolean

  protected def ifImplementation[A, B, C](value: IsImplementation match {
    case true => A
    case false => B
  })(whenImplementation: A => C, whenInterface: Either[A, B] => C): C

  protected def dummyImplementationValue: IsImplementation match {
    case true => Unit
    case false => Unit
  }

  private def processImplementation[Impl, T](isInline: Boolean, impl: IsImplementation match {
    case true => Comp[Impl]
    case false => Comp[Option[Impl]]
  })(f: Impl => Comp[T]): Comp[Option[T]] =
    ifImplementation(impl)(
      whenImplementation = _.flatMap(f).asSome,
      whenInterface = { implEither =>
        val impl = implEither match {
          case Left(l) => l.asSome
          case Right(r) => r
        }

        if isInline then
          impl.flatMap {
            case Some(impl) => f(impl).asSome
            case None => ZIO.none
          }
        else
          ZIO.none
      },
    )

  val currentTube: ArTube & HasImplementation[IsImplementation]


  import context.ExprContext.{ArExpr, ClassResult, ExprConstructor, FunctionResult, LocalVariable, ParameterVariableOwner, TraitResult, Variable, WrapExpr}



  private trait DefinitionTubeLookup[A <: Definition] {
    def getTube(a: A): ArTube
  }

  private def registerReference[A <: Definition](a: A)(using ids: IdentifierMaps[A, IsImplementation], tubeLookup: DefinitionTubeLookup[A]): UIO[Unit] =
    if tubeLookup.getTube(a).tubeName == currentTube.tubeName then
      ZIO.unit
    else
      ids.definitionIds.size.flatMap { defsSize =>
        if defsSize > 0 then
          ZSTM.die(IllegalStateException("Cannot register reference after definition has been registered"))
        else
          ids.referenceIds.contains(a).flatMap {
            case true => ZSTM.unit
            case false =>
              ids.referenceIds.size.flatMap { id =>
                ids.referenceIds.put(a, id) *>
                  ids.references.update(_ :+ a)
              }
          }
      }.commit

  private def registerDefinition[A <: Definition](a: A & HasImplementation[IsImplementation])(using ids: IdentifierMaps[A, IsImplementation]): UIO[BigInt] =
    ids.referenceIds.size.flatMap { numRefs =>
      ids.definitionIds.size.map { numDefs =>
        (numRefs : BigInt) + numDefs
      }
    }
      .tap { id =>
        ids.definitionIds.put(a, id) *>
          ids.definitionLookup.put(id, a)
      }
      .commit

  private def lookupDefinition[A <: Definition](id: BigInt)(using ids: IdentifierMaps[A, IsImplementation]): UIO[A & HasImplementation[IsImplementation]] =
    ids.definitionLookup.get(id).commit.flatMap {
      case Some(a) => ZIO.succeed(a)
      case None => ZIO.die(NoSuchElementException())
    }

  private def getIdOf[A <: Definition](a: A)(using ids: IdentifierMaps[A, IsImplementation]): UIO[BigInt] =
    ids.referenceIds.get(a)
      .flatMap {
        case Some(id) => ZSTM.some(id)
        case None => ids.definitionIds.get(a)
      }
      .flatMap {
        case Some(id) => ZSTM.succeed(id)
        case None => ZSTM.die(NoSuchElementException())
      }
      .commit


  given classIds: IdentifierMaps[ArClass, IsImplementation]
  private given DefinitionTubeLookup[ArClass] with
    override def getTube(a: ArClass): ArTube = ArClassC.getOwningTube(a.owner)
  end given

  given traitIds: IdentifierMaps[ArTrait, IsImplementation]
  private given DefinitionTubeLookup[ArTrait] with
    override def getTube(a: ArTrait): ArTube = ArTraitC.getOwningTube(a.owner)
  end given

  given functionIds: IdentifierMaps[ArFunc, IsImplementation]
  private given DefinitionTubeLookup[ArFunc] with
    override def getTube(a: ArFunc): ArTube = ArFuncC.getOwningTube(a.owner)
  end given

  given methodIds: IdentifierMaps[ArMethod, IsImplementation]
  private given DefinitionTubeLookup[ArMethod] with
    override def getTube(a: ArMethod): ArTube = ArMethodC.getOwningTube(a.owner)
  end given

  given classCtorIds: IdentifierMaps[ClassConstructor, IsImplementation]
  private given DefinitionTubeLookup[ClassConstructor] with
    override def getTube(a: ClassConstructor): ArTube = ClassConstructorC.getOwningTube(a.owner)
  end given




  private lazy val sigEraser = SignatureEraser(context)


  def scanTubeRefs: Comp[Unit] =
    ZIO.foreachDiscard(currentTube.modulePaths) { modulePath =>
      currentTube.module(modulePath).flatMap(scanModuleRefs)
    }

  private def scanModuleRefs(module: ArModule & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      val exports = module.allExports(Set.empty).run
      for export_ <- exports do
        export_ match
          case ModuleElementC.ClassElement(arClass) => scanClassRefs(arClass).run
          case ModuleElementC.TraitElement(arTrait) => scanTraitRefs(arTrait).run
          case ModuleElementC.FunctionElement(arFunc) => scanFuncRefs(arFunc).run
          case ModuleElementC.ExportedElement(_, _, inner) => scanExportedElementRefs(inner).run
        end match
    }

  private def scanExportedElementRefs(value: ModuleElement[?]): Comp[Unit] =
    defer {
      value match {
        case ModuleElementC.ClassElement(arClass) =>
          registerReference[ArClass](arClass).run
          scanClassRefsSig(arClass).run

        case ModuleElementC.TraitElement(arTrait) =>
          registerReference[ArTrait](arTrait).run
          scanTraitRefsSig(arTrait).run

        case ModuleElementC.FunctionElement(arFunc) =>
          registerReference[ArFunc](arFunc).run
          scanFuncRefsSig(arFunc).run

        case ModuleElementC.ExportedElement(_, _, inner) =>
          scanExportedElementRefs(inner).run
      }
    }

  private def scanClassRefs(arClass: ArClass & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      scanClassRefsSig(arClass).run

      for field <- arClass.fields.run do
        scanVariableRefs(field).run

      for methodGroup <- arClass.methods.run.values do
        for method <- methodGroup do
          scanMethodRefs(method).run

      for methodGroup <- arClass.staticMethods.run.values do
        for method <- methodGroup do
          scanMethodRefs(method).run

      for constructors <- arClass.constructors.run do
        scanClassConstructorRefs(constructors).run
    }

  private def scanClassRefsSig(arClass: ArClass): Comp[Unit] =
    defer {
      val sig = arClass.signature.run
      for t <- sig.parameterTypes do
        scanExprRefs(t).run

      val res = sig.unsubstitutedResult
      scanExprRefs(res.classTypeSuperType).run
      for baseClass <- res.baseClass.run.toList do
        scanArExprRefs(baseClass).run
      for baseTrait <- res.baseTraits.run do
        scanArExprRefs(baseTrait).run
    }

  private def scanTraitRefs(arTrait: ArTrait & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      scanTraitRefsSig(arTrait).run

      for methodGroup <- arTrait.methods.run.values do
        for method <- methodGroup do
          scanMethodRefs(method).run

      for methodGroup <- arTrait.staticMethods.run.values do
        for method <- methodGroup do
          scanMethodRefs(method).run
    }

  private def scanTraitRefsSig(arTrait: ArTrait): Comp[Unit] =
    defer {
      val sig = arTrait.signature.run
      for t <- sig.parameterTypes do
        scanExprRefs(t).run

      val res = sig.unsubstitutedResult
      scanExprRefs(res.traitTypeSuperType).run
      for baseTrait <- res.baseTraits.run do
        scanArExprRefs(baseTrait).run
    }

  private def scanFuncRefs(arFunc: ArFunc & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      scanFuncRefsSig(arFunc).run

      processImplementation(arFunc.isInline, arFunc.implementation) {
        case body: FunctionImplementationC.ExpressionBody => scanExprRefs(body.body)
        case _: FunctionImplementationC.External => ZIO.unit
      }.unit.run
    }

  private def scanFuncRefsSig(arFunc: ArFunc): Comp[Unit] =
    defer {
      val sig = arFunc.signature.run
      for t <- sig.parameterTypes do
        scanExprRefs(t).run

      val sigResult = sig.unsubstitutedResult

      scanExprRefs(sigResult.returnType).run

      for clause <- sigResult.ensuresClauses do
        scanExprRefs(clause).run
    }

  private def scanMethodRefs(arMethod: ArMethod & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      arMethod.owner match {
        case OwnedByClassC(arClass, _, _) => registerReference[ArClass](arClass).run
        case OwnedByClassStaticC(arClass, _, _) => registerReference[ArClass](arClass).run
        case OwnedByTraitC(arTrait, _, _) => registerReference[ArTrait](arTrait).run
        case OwnedByTraitStaticC(arTrait, _, _) => registerReference[ArTrait](arTrait).run
      }

      scanMethodRefsSig(arMethod).run

      processImplementation(arMethod.isInline, arMethod.implementation) {
        case body: MethodImplementationC.ExpressionBody => scanExprRefs(body.body)
        case _: MethodImplementationC.External => ZIO.unit
        case _: MethodImplementationC.Abstract => ZIO.unit
      }.unit.run
    }

  private def scanMethodRefsSig(arMethod: ArMethod): Comp[Unit] =
    defer {
      val sig = arMethod.signatureUnsubstituted.run
      for t <- sig.parameterTypes do
        scanExprRefs(t).run

      val sigResult = sig.unsubstitutedResult

      scanExprRefs(sigResult.returnType).run

      for clause <- sigResult.ensuresClauses do
        scanExprRefs(clause).run
    }

  private def scanClassConstructorRefs(classConstructor: ClassConstructor & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      registerReference[ArClass](classConstructor.owner.arClass).run

      val sig = classConstructor.signatureUnsubstituted.run
      for t <- sig.parameterTypes do
        scanExprRefs(t).run

      processImplementation(false, classConstructor.implementation) {
        case body: ClassConstructorImplementationC.ExpressionBody => scanClassConstructorImpl(body)
        case _: ClassConstructorImplementationC.External => ZIO.unit
      }.unit.run
    }

  private def scanClassConstructorRefsSig(classConstructor: ClassConstructor): Comp[Unit] =
    defer {
      val sig = classConstructor.signatureUnsubstituted.run
      for t <- sig.parameterTypes do
        scanExprRefs(t).run
    }

  private def scanVariableRefs(variable: Variable): Comp[Unit] =
    scanExprRefs(variable.varType)

  private def scanArExprRefs(expr: ArExpr[ExprConstructor]): Comp[Unit] =
    defer {
      expr.constructor match {
        case ExprConstructor.BindVariable(variable) => scanVariableRefs(variable).run
        case ExprConstructor.ClassConstructorCall(classCtor) =>
          registerReference[ClassConstructor](classCtor).run
          scanClassConstructorRefsSig(classCtor).run

        case ExprConstructor.EnsureExecuted =>

        case ExprConstructor.FunctionCall(function) =>
          registerReference[ArFunc](function).run
          scanFuncRefsSig(function)

        case ExprConstructor.FunctionObjectCall =>

        case ExprConstructor.IfElse(whenTrue, whenFalse) =>
          for wt <- whenTrue.toList do
            scanVariableRefs(wt).run

          for wf <- whenFalse.toList do
            scanVariableRefs(wf).run

        case ExprConstructor.LoadConstantBool(b) =>

        case ExprConstructor.LoadConstantInt(i) =>

        case ExprConstructor.LoadConstantString(s) =>

        case ExprConstructor.LoadLambda(argVariable) =>
          scanVariableRefs(argVariable).run

        case ExprConstructor.LoadTuple =>

        case ExprConstructor.LoadTupleElement(index) =>

        case ExprConstructor.LoadVariable(variable) =>
          scanVariableRefs(variable).run

        case ExprConstructor.MethodCall(method) =>
          registerReference[ArMethod](method).run
          scanMethodRefsSig(method).run

        case ExprConstructor.PatternMatch(patterns) => ???

        case ExprConstructor.Proving(witnesses) =>
          for witness <- witnesses do
            scanVariableRefs(witness).run

        case ExprConstructor.RaiseException =>

        case ExprConstructor.Sequence =>

        case ExprConstructor.StoreVariable(variable) =>
          scanVariableRefs(variable).run

        case ExprConstructor.TypeN =>

        case ExprConstructor.OmegaTypeN(level) =>

        case ExprConstructor.AnyType =>

        case ExprConstructor.TraitType(arTrait) =>
          registerReference[ArTrait](arTrait).run
          scanTraitRefsSig(arTrait).run

        case ExprConstructor.ClassType(arClass) =>
          registerReference[ArClass](arClass).run
          scanClassRefsSig(arClass).run

        case ExprConstructor.FunctionType =>

        case ExprConstructor.UnionType =>

        case ExprConstructor.IntersectionType =>

        case ExprConstructor.ExistentialType(variable) =>
          scanVariableRefs(variable).run

        case ExprConstructor.ConjunctionType =>

        case ExprConstructor.DisjunctionType =>

        case ExprConstructor.NeverType =>

        case ExprConstructor.SubtypeWitnessType =>

        case ExprConstructor.EqualTo =>

        case ExprConstructor.AssumeErasedValue =>
      }

      for arg <- expr.constructor.argsToExprs(expr.args) do
        scanExprRefs(arg)
    }


  private def scanExprRefs(expr: WrapExpr): Comp[Unit] =
    expr match {
      case WrapExpr.OfExpr(expr) => scanArExprRefs(expr)
      case WrapExpr.OfHole(hole) => hole
    }

  private def scanClassConstructorImpl(impl: ClassConstructorImplementationC.ExpressionBody & HasContext[context.type]): Comp[Unit] =
    defer {
      for preInit <- impl.preInitialization do
        preInit match
          case Left(expr) => scanExprRefs(expr).run
          case Right(fieldInit) =>
            scanVariableRefs(fieldInit.field).run
            scanExprRefs(fieldInit.value).run
        end match

      for baseCall <- impl.baseConstructorCall.baseCall.toList do
        scanArExprRefs(baseCall).run

      scanExprRefs(impl.postInitialization).run
    }


  def scanTubeDefs: Comp[Unit] =
    ZIO.foreachDiscard(currentTube.modulePaths) { modulePath =>
      currentTube.module(modulePath).flatMap(scanModuleDefs)
    }

  private def scanModuleDefs(module: ArModule & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      val exports = module.allExports(Set.empty).run
      for export_ <- exports do
        export_ match
          case ModuleElementC.ClassElement(arClass) =>
            registerDefinition[ArClass](arClass).run
            scanClassDefs(arClass).run

          case ModuleElementC.TraitElement(arTrait) =>
            registerDefinition[ArTrait](arTrait).run
            scanTraitDefs(arTrait).run

          case ModuleElementC.FunctionElement(arFunc) =>
            registerDefinition[ArFunc](arFunc).run

          case ModuleElementC.ExportedElement(_, _, _) =>
        end match
    }

  private def scanClassDefs(arClass: ArClass & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      for methodGroup <- arClass.methods.run.values do
        for method <- methodGroup do
          registerDefinition[ArMethod](method).run

      for methodGroup <- arClass.staticMethods.run.values do
        for method <- methodGroup do
          registerDefinition[ArMethod](method).run

      for constructors <- arClass.constructors.run do
        registerDefinition[ClassConstructor](constructors).run
    }

  private def scanTraitDefs(arTrait: ArTrait & HasImplementation[IsImplementation]): Comp[Unit] =
    defer {
      for methodGroup <- arTrait.methods.run.values do
        for method <- methodGroup do
          registerDefinition[ArMethod](method).run

      for methodGroup <- arTrait.staticMethods.run.values do
        for method <- methodGroup do
          registerDefinition[ArMethod](method).run
    }



  def emitMetadata(): Comp[t.Metadata] =
    def buildRefsComp[T <: Definition, Msg]
    (
      ids: IdentifierMaps[T, IsImplementation],
      getModule: T => ArModule,
      getSig: T => Comp[ErasedSignature],
      build: (T, ArModule, t.ErasedSignature) => Comp[Msg]
    ): Comp[Seq[Msg]] =
      for
        refEntries <- ids.references.get.commit
        messages <- ZIO.foreach(refEntries) { t =>
          for
            sig <- getSig(t)
            msg <- build(t, getModule(t), getErasedSignature(sig))
          yield msg
        }
      yield messages

    def buildRefs[T <: Definition, Msg]
    (
      ids: IdentifierMaps[T, IsImplementation],
      getModule: T => ArModule,
      getSig: T => Comp[ErasedSignature],
      build: (T, ArModule, t.ErasedSignature) => Msg
    ): Comp[Seq[Msg]] =
      buildRefsComp(ids, getModule, getSig, (t, module, sig) => ZIO.succeed(build(t, module, sig)))


    for
      resourceRecorder <- MetadataResourceRecorder.make[context.Env, context.Error]

      options <- ifImplementation(currentTube.options)(
        whenImplementation = options => context.optionsCodec.encode(resourceRecorder)(options).map(t.TomlConverter.encodeToml).asSome,
        whenInterface = _ => ZIO.none,
      )

      resources <- resourceRecorder.resources.keys.commit

      methodRefs <- buildRefsComp(
        ids = methodIds,
        getModule = arMethod => ArMethodC.getOwningModule(arMethod.owner),
        getSig = _.signatureUnsubstituted.flatMap(sigEraser.erasedWithResult),
        build = (arMethod, _, sig) =>
          (
            arMethod.owner match {
              case OwnedByClassC(arClass, name, _) =>
                for
                  classId <- getIdOf[ArClass](arClass)
                yield (t.ExternalReferenceMethod.OwnerType.Class, classId, name)

              case OwnedByClassStaticC(arClass, name, _) =>
                for
                  classId <- getIdOf[ArClass](arClass)
                yield (t.ExternalReferenceMethod.OwnerType.ClassStatic, classId, name)

              case OwnedByTraitC(arTrait, name, _) =>
                for
                  traitId <- getIdOf[ArTrait](arTrait)
                yield (t.ExternalReferenceMethod.OwnerType.Trait, traitId, name)

              case OwnedByTraitStaticC(arTrait, name, _) =>
                for
                  traitId <- getIdOf[ArTrait](arTrait)
                yield (t.ExternalReferenceMethod.OwnerType.TraitStatic, traitId, name)
            }
            ).map { case (ownerType, ownerId, name) =>
            t.ExternalReferenceMethod(
              ownerType = ownerType,
              ownerId = ownerId,
              identifier = name.map(getIdentifier),
              signature = sig,
            )
          }
      )

      classCtorRefs <- buildRefsComp(
        ids = classCtorIds,
        getModule = ctor => ClassConstructorC.getOwningModule(ctor.owner),
        getSig = _.signatureUnsubstituted.flatMap(sigEraser.erasedNoResult),
        build = (ctor, _, sig) =>
          for
            classId <- getIdOf[ArClass](ctor.owner.arClass)
          yield t.ExternalReferenceClassConstructor(
            ownerId = classId,
            signature = sig,
          )
      )

      classRefs <- buildRefs(
        ids = classIds,
        getModule = arClass => ArClassC.getOwningModule(arClass.owner),
        getSig = _.signature.flatMap(sigEraser.erasedNoResult),
        build = (arClass, module, sig) => t.ExternalReference(
          specifier = t.ImportSpecifier(
            tube = module.tube.tubeName.name.toList,
            module = module.moduleName.path.ids,
            name = arClass.owner.ownedName.map(getIdentifier),
            signature = sig,
          )
        ),
      )

      traitRefs <- buildRefs(
        ids = traitIds,
        getModule = arTrait => ArTraitC.getOwningModule(arTrait.owner),
        getSig = _.signature.flatMap(sigEraser.erasedNoResult),
        build = (arTrait, module, sig) => t.ExternalReference(
          specifier = t.ImportSpecifier(
            tube = module.tube.tubeName.name.toList,
            module = module.moduleName.path.ids,
            name = arTrait.owner.ownedName.map(getIdentifier),
            signature = sig,
          )
        ),
      )

      functionRefs <- buildRefs(
        ids = functionIds,
        getModule = func => ArFuncC.getOwningModule(func.owner),
        getSig = _.signature.flatMap(sigEraser.erasedWithResult),
        build = (func, module, sig) => t.ExternalReference(
          specifier = t.ImportSpecifier(
            tube = module.tube.tubeName.name.toList,
            module = module.moduleName.path.ids,
            name = func.owner.ownedName.map(getIdentifier),
            signature = sig,
          )
        ),
      )

      referencedTubes =
        (
          classRefs.iterator ++
            traitRefs.iterator ++
            functionRefs.iterator
          )
          .map { refs => refs.specifier.tube }
          .distinct
          .map(t.TubeName.apply)
          .toSeq

    yield t.Metadata(
      name = t.TubeName(currentTube.tubeName.name.toList),
      `type` = ifImplementation(dummyImplementationValue)(
        whenImplementation = _ => t.TubeType.Implementation,
        whenInterface = _ => t.TubeType.Interface
      ),
      options = options,
      optionResources = resources,
      modules = currentTube.modulePaths
        .iterator
        .map { modulePath => t.ModulePath(modulePath.ids) }
        .toSeq,

      references = referencedTubes,
      externalClasses = classRefs,
      externalTraits = traitRefs,
      externalFunctions = functionRefs,
      externalMethods = methodRefs,
      externalClassConstructors = classCtorRefs,
    )
  end emitMetadata

  def emitModule(modulePath: ModulePath): Comp[t.ModuleDefinition] =
    currentTube.module(modulePath).flatMap[context.Env, context.Error, t.ModuleDefinition] { module =>
      for
        exports <- module.allExports(Set.empty)
        elements <- ZIO.foreach(exports.groupBy(_.name).toSeq) { (name, exports) =>
          val (exported, decl) = exports.partitionMap {
            case ModuleElementC.ClassElement(arClass) =>
              Right(
                for
                  id <- registerDefinition[ArClass](arClass)
                yield t.ModuleDefinition.ElementDeclaration(
                  id = id,
                  `type` = t.ModuleDefinition.ElementDeclaration.Type.Class,
                  accessModifier = getAccessModifier(arClass.owner.accessModifier),
                )
              )

            case ModuleElementC.TraitElement(arTrait) =>
              Right(
                for
                  id <- registerDefinition[ArTrait](arTrait)
                yield t.ModuleDefinition.ElementDeclaration(
                  id = id,
                  `type` = t.ModuleDefinition.ElementDeclaration.Type.Trait,
                  accessModifier = getAccessModifier(arTrait.owner.accessModifier),
                )
              )

            case ModuleElementC.FunctionElement(func) =>
              Right(
                for
                  id <- registerDefinition[ArFunc](func)
                yield t.ModuleDefinition.ElementDeclaration(
                  id = id,
                  `type` = t.ModuleDefinition.ElementDeclaration.Type.Function,
                  accessModifier = getAccessModifier(func.owner.accessModifier),
                )
              )

            case ModuleElementC.ExportedElement(module, name, _) =>
              Left(
                t.ModuleDefinition.ElementExported(
                  module = getModuleName(module.moduleName),
                  name = name.map(getIdentifier),
                )
              )
          }

          ZIO.foreach(decl)(identity).map { decl =>
            t.ModuleDefinition.NameGroup(
              name = name.map(getIdentifier),
              declaredElements = decl,
              exportedElements = exported,
            )
          }
        }

      yield t.ModuleDefinition(
        elements = elements,
      )
    }

  private def getErasedSignature(sig: ErasedSignature): t.ErasedSignature =
    sig match {
      case ErasedSignatureWithResult(params, result) =>
        t.ErasedSignature(
          params.map(getErasedSignatureType),
          Some(getErasedSignatureType(result)),
        )

      case ErasedSignatureNoResult(params) =>
        t.ErasedSignature(params.map(getErasedSignatureType), None)
    }

  private def getErasedSignatureType(sigType: ErasedSignatureType): t.ErasedSignatureType =
    sigType match {
      case ErasedSignatureType.Class(classImport, args) =>
        t.ErasedSignatureType(
          t.ErasedSignatureType.Constructor._Class(getImportSpecifier(classImport)),
          args.map(getErasedSignatureType),
        )

      case ErasedSignatureType.Trait(traitImport, args) =>
        t.ErasedSignatureType(
          t.ErasedSignatureType.Constructor.Trait(getImportSpecifier(traitImport)),
          args.map(getErasedSignatureType),
        )

      case ErasedSignatureType.Function(input, output) =>
        t.ErasedSignatureType(
          t.ErasedSignatureType.Constructor.Function(Empty()),
          Seq(
            getErasedSignatureType(input),
            getErasedSignatureType(output),
          ),
        )

      case ErasedSignatureType.Tuple(elements) =>
        t.ErasedSignatureType(
          t.ErasedSignatureType.Constructor.Tuple(Empty()),
          elements.map(getErasedSignatureType),
        )

      case ErasedSignatureType.Erased =>
        t.ErasedSignatureType(
          t.ErasedSignatureType.Constructor.Erased(Empty()),
          Seq.empty,
        )
    }

  private def getImportSpecifier(specifier: ImportSpecifier): t.ImportSpecifier =
    t.ImportSpecifier(
      tube = specifier.tube.name.toList,
      module = specifier.module.ids,
      name = specifier.name.map(getIdentifier),
      signature = getErasedSignature(specifier.signature)
    )


  private def getFlags[T, Enum <: GeneratedEnum](flags: (Enum, Boolean)*): Int =
    flags.foldLeft(0) { case (acc, (enumValue, flagValue)) =>
      acc | (if flagValue then enumValue.value else 0)
    }


  private def emitSignature[Res, A](sig: Signature[WrapExpr, Res])(f: (Seq[t.Parameter], Res) => Comp[A]): Comp[A] =
    def impl(sig: Signature[WrapExpr, Res])(params: Seq[t.Parameter]): Comp[A] =
      sig match {
        case Signature.Parameter(listType, isErased, name, paramType, next) =>
          getExprWithVariables(paramType).flatMap { paramType =>
            val param = t.Parameter(
              listType = listType match {
                case FunctionParameterListType.NormalList => t.Parameter.ListType.NormalList
                case FunctionParameterListType.InferrableList => t.Parameter.ListType.InferrableList
                case FunctionParameterListType.InferrableList2 => t.Parameter.ListType.InferrableList2
                case FunctionParameterListType.RequiresList => t.Parameter.ListType.RequiresList
              },
              flags = (if isErased then t.Parameter.Flags.Erased.value else 0),
              name = name.map(getIdentifier),
              paramType = paramType
            )
            impl(next)(params :+ param)
          }

        case Signature.Result(resultType) =>
          f(params, resultType)
      }

    impl(sig)(Seq.empty)

  private def getModuleName(moduleName: ModuleName): t.ModuleName =
    t.ModuleName(
      tube = moduleName.tubeName.name.toList,
      module = moduleName.path.ids,
    )




  def emitClass(id: BigInt): Comp[t.ClassDefinition] =
    for
      arClass <- lookupDefinition[ArClass](id)
      _ <- arClass.validate

      flags = getFlags(
        t.ClassDefinition.Flags.Abstract -> arClass.isAbstract,
        t.ClassDefinition.Flags.Sealed -> arClass.isSealed,
        t.ClassDefinition.Flags.Open -> arClass.isOpen,
      )

      sig <- arClass.signature
      signature <- emitSignature(sig) { case (params, ClassResult(classTypeSuperType, baseClass, baseTraits)) =>
        for
          classSuperType <- getExprWithVariables(classTypeSuperType)
          baseClass <- baseClass
          baseClass <- ZIO.foreach(baseClass)(emitClassType)

          baseTraits <- baseTraits
          baseTraits <- ZIO.foreach(baseTraits)(emitTraitType)
        yield t.ClassDefinition.Signature(
          parameters = params,
          classTypeSuperType = classSuperType,
          baseClass = baseClass,
          baseTraits = baseTraits,
        )
      }

      fields <- arClass.fields
      fields <- ZIO.foreach(fields) { field =>
        for
          fieldType <- getExprWithVariables(field.varType)
        yield t.ClassField(
          name = getIdentifier(field.name.get),
          mutability = getMutability(field.isMutable),
          fieldType = fieldType,
        )
      }

      methods <- arClass.methods
      methods <- ZIO.foreach(methods.toSeq)(createMethodMemberGroup)

      staticMethods <- arClass.staticMethods
      staticMethods <- ZIO.foreach(staticMethods.toSeq)(createMethodMemberGroup)

      constructors <- arClass.constructors
      constructors <- ZIO.foreach(constructors) { ctor =>
        for
          id <- getIdOf[ClassConstructor](ctor)
        yield t.ClassConstructorMember(
          id,
          getAccessModifier(ctor.owner.accessModifier),
        )
      }

    yield t.ClassDefinition(
      flags = flags,
      signature = signature,
      fields = fields,
      methods = methods,
      staticMethods = staticMethods,
      constructors = constructors,
    )

  def getVTableDiff(id: BigInt): Comp[t.VTable] =
    def convertInstanceType(inst: ExprConstructor.MethodCallOwnerType): Comp[t.Expr] =
      inst match {
        case ExprConstructor.MethodCallOwnerType.OwnedByClass(arClass) => getExprWithVariables(WrapExpr.OfExpr(arClass))
        case ExprConstructor.MethodCallOwnerType.OwnedByTrait(arTrait) => getExprWithVariables(WrapExpr.OfExpr(arTrait))
      }

    def convertEntry(slot: ArMethod, entry: context.VT.VTableEntry): Comp[t.VTableEntry] =
      for
        slotId <- getIdOf(slot)
        slotOwner <- convertInstanceType(entry.slotInstanceType)

        impl <- entry.impl match {
          case context.VT.VTableEntryMethod(method, methodInstanceType) =>
            for
              id <- getIdOf(method)
              inst <- convertInstanceType(methodInstanceType)
            yield t.VTableEntry.Impl.Method(t.VTableEntryMethod(id, inst))

          case context.VT.VTableEntryAmbiguous(methods) =>
            ZIO.foreach(methods.toSeq)(getIdOf)
              .map(t.VTableEntryAmbiguous.apply andThen t.VTableEntry.Impl.Ambiguous.apply)

          case context.VT.VTableEntryAbstract =>
            ZIO.succeed(t.VTableEntry.Impl.Abstract(Empty()))

        }

      yield t.VTableEntry(
        methodId = slotId,
        name = entry.name.map(getIdentifier),
        owner = slotOwner,
        impl = impl,
      )

    lookupDefinition[ArClass](id)
      .tap[context.Env, context.Error](_.validate)
      .flatMap[context.Env, context.Error, context.VT.VTable](_.vtableDiff)
      .flatMap { vt =>
        ZIO.foreach(vt.methodMap.toSeq)(convertEntry)
      }
      .map(t.VTable.apply)
  end getVTableDiff


  def emitTrait(id: BigInt): Comp[t.TraitDefinition] =
    for
      arTrait <- lookupDefinition[ArTrait](id)
      _ <- arTrait.validate

      flags = getFlags(
        t.TraitDefinition.Flags.Sealed -> arTrait.isSealed,
      )

      sig <- arTrait.signature
      signature <- emitSignature(sig) { case (params, TraitResult(traitTypeSuperType, baseTraits)) =>
        for
          traitSuperType <- getExprWithVariables(traitTypeSuperType)
          baseTraits <- baseTraits
          baseTraits <- ZIO.foreach(baseTraits)(emitTraitType)
        yield t.TraitDefinition.Signature(
          parameters = params,
          traitTypeSuperType = traitSuperType,
          baseTraits = baseTraits,
        )
      }

      methods <- arTrait.methods
      methods <- ZIO.foreach(methods.toSeq)(createMethodMemberGroup)

      staticMethods <- arTrait.staticMethods
      staticMethods <- ZIO.foreach(staticMethods.toSeq)(createMethodMemberGroup)

    yield t.TraitDefinition(
      flags = flags,
      signature = signature,
      methods = methods,
      staticMethods = staticMethods,
    )



  def emitFunction(id: BigInt): Comp[t.FunctionDefinition] =
    for
      func <- lookupDefinition[ArFunc](id)
      _ <- func.validate

      flags = getFlags(
        t.FunctionDefinition.Flags.Erased -> func.isErased,
        t.FunctionDefinition.Flags.Proof -> func.isProof,
      )

      sig <- func.signature
      signature <- emitSignature(sig)(emitFunctionSignature)

      body <- processImplementation(func.isInline, func.implementation) {
        case impl: FunctionImplementationC.ExpressionBody =>
          for
            body <- getExprWithVariables(impl.body)
          yield t.FunctionBody(t.FunctionBody.Value.ExpressionBody(body))

        case impl: FunctionImplementationC.External =>
          ZIO.succeed(t.FunctionBody(t.FunctionBody.Value.ExternalImplementation(impl.name)))
      }

    yield t.FunctionDefinition(
      flags = flags,
      signature = signature,
      effects = t.EffectInfo(
        isPure = func.purity,
      ),
      body = body,
    )

  def getExternFunctionImplementation(id: BigInt): Comp[context.ExternFunctionImplementation] =
    for
      func <- lookupDefinition[ArFunc](id)
      impl <- processImplementation(func.isInline, func.implementation) {
        case impl: FunctionImplementationC.External =>
          ZIO.some(impl.impl: context.ExternFunctionImplementation)

        case _ =>
          ZIO.none
      }

      impl <- ZIO.fromEither(impl.flatten.toRight(DiagnosticError.InternalCompilerError("Expected extern function implementation")))
    yield impl

  def emitFunctionSignature(params: Seq[t.Parameter], functionResult: FunctionResult): Comp[t.FunctionSignature] =
    for
      returnType <- getExprWithVariables(functionResult.returnType)
      ensuresClauses <- ZIO.foreach(functionResult.ensuresClauses)(getExprWithVariables)
    yield t.FunctionSignature(
      parameters = params,
      returnType = returnType,
      ensuresClauses = ensuresClauses,
    )


  def createMethodMemberGroup(name: Option[IdentifierExpr], methods: Seq[ArMethod & HasImplementation[IsImplementation]]): Comp[t.MethodMemberGroup] =
    ZIO.foreach(methods)(createMethodMember(name)).map { methods =>
      t.MethodMemberGroup(name.map(getIdentifier), methods)
    }

  def createMethodMember(name: Option[IdentifierExpr])(method: ArMethod & HasImplementation[IsImplementation]): Comp[t.MethodMember] =
    for
      id <- getIdOf[ArMethod](method)
    yield t.MethodMember(
      id = id,
      accessModifier = getAccessModifier(ArMethodC.getAccessModifier(method.owner))
    )



  def emitMethod(id: BigInt): Comp[t.MethodDefinition] =
    for
      method <- lookupDefinition[ArMethod](id)
      _ <- method.validate

      flags = getFlags(
        t.MethodDefinition.Flags.Virtual -> (method.isVirtual && !(method.isAbstract || method.isImplicitOverride)),
        t.MethodDefinition.Flags.Abstract -> method.isAbstract,
        t.MethodDefinition.Flags.AutoOverride -> method.isImplicitOverride,
        t.MethodDefinition.Flags.Final -> method.isFinal,
        t.MethodDefinition.Flags.Erased -> method.isErased,
        t.MethodDefinition.Flags.Proof -> method.isProof,
      )

      sig <- method.signatureUnsubstituted
      signature <- emitSignature(sig)(emitFunctionSignature)

      body <- processImplementation(method.isInline, method.implementation) {
        case impl: MethodImplementationC.ExpressionBody =>
          for
            body <- getExprWithVariables(impl.body)
          yield Some(t.FunctionBody(t.FunctionBody.Value.ExpressionBody(body)))

        case impl: MethodImplementationC.External =>
          ZIO.succeed(Some(t.FunctionBody(t.FunctionBody.Value.ExternalImplementation(impl.name))))

        case impl: MethodImplementationC.Abstract =>
          ZIO.succeed(None)
      }

    yield t.MethodDefinition(
      flags = flags,
      signature = signature,
      effects = t.EffectInfo(
        isPure = method.purity,
      ),
      body = body.flatten,
      instanceVariableName = method.instanceVariableName.map(getIdentifier),
    )

  def getExternMethodImplementation(id: BigInt): Comp[context.ExternMethodImplementation] =
    for
      method <- lookupDefinition[ArMethod](id)
      impl <- processImplementation(method.isInline, method.implementation) {
        case impl: MethodImplementationC.External =>
          ZIO.some(impl.impl : context.ExternMethodImplementation)

        case _ =>
          ZIO.none
      }

      impl <- ZIO.fromEither(impl.flatten.toRight(DiagnosticError.InternalCompilerError("Expected extern method implementation")))
    yield impl

  def emitClassConstructor(id: BigInt): Comp[t.ClassConstructorDefinition] =
    for
      classCtor <- lookupDefinition[ClassConstructor](id)
      sig <- classCtor.signatureUnsubstituted
      signature <- emitSignature(sig) { (paramTypes, _) =>
        ZIO.succeed(t.ClassConstructorDefinition.Signature(paramTypes))
      }

      body <- processImplementation(false, classCtor.implementation) {
        case impl: ClassConstructorImplementationC.ExpressionBody =>
          withVariables {
            def getPreInit(stmt: Either[WrapExpr, ClassConstructorImplementationC.FieldInitializationStatement & HasContext[context.type]]): Comp[t.ClassConstructorDefinition.PreInitializationStatement] =
              stmt match {
                case Left(expr) =>
                  for
                    expr <- getExpr(expr)
                  yield t.ClassConstructorDefinition.PreInitializationStatement(
                    t.ClassConstructorDefinition.PreInitializationStatement.Value.Expr(expr)
                  )
                case Right(fieldInit) =>
                  for
                    value <- getExpr(fieldInit.value)
                  yield t.ClassConstructorDefinition.PreInitializationStatement(
                    t.ClassConstructorDefinition.PreInitializationStatement.Value.FieldInit(
                      t.ClassConstructorDefinition.FieldInitializationStatement(
                        field = getIdentifier(fieldInit.field.name.get),
                        value = value,
                      )
                    )
                  )
              }

            for
              preInit <- ZIO.foreach(impl.preInitialization)(getPreInit)
              baseCall <- ZIO.foreach(impl.baseConstructorCall.baseCall) { call =>
                getExpr(WrapExpr.OfExpr(call))
              }
              instanceVar <- getVariableDeclaration(impl.baseConstructorCall.instanceVariable)
              postInit <- getExpr(impl.postInitialization)
            yield t.ClassConstructorDefinition.ExpressionBody(
              preInitialization = preInit,
              baseConstructorCall = baseCall,
              instanceVariable = instanceVar,
              postInitialization = postInit,
            )
          }.map { body =>
            t.ClassConstructorDefinition.Body(
              t.ClassConstructorDefinition.Body.Value.ExpressionBody(body)
            )
          }

        case impl: ClassConstructorImplementationC.External =>
          ZIO.succeed(t.ClassConstructorDefinition.Body(t.ClassConstructorDefinition.Body.Value.ExternalImplementation(impl.name)))
      }

    yield t.ClassConstructorDefinition(
      flags = 0,
      signature = signature,
      effects = t.EffectInfo(
        isPure = classCtor.purity,
      ),
      body = body,
    )

  def getExternClassConstructorImplementation(id: BigInt): Comp[context.ExternClassConstructorImplementation] =
    for
      ctor <- lookupDefinition[ClassConstructor](id)
      impl <- processImplementation(false, ctor.implementation) {
        case impl: ClassConstructorImplementationC.External =>
          ZIO.some(impl.impl: context.ExternClassConstructorImplementation)

        case _ =>
          ZIO.none
      }

      impl <- ZIO.fromEither(impl.flatten.toRight(DiagnosticError.InternalCompilerError("Expected extern class constructor implementation")))
    yield impl

  def getIdentifier(id: IdentifierExpr): t.Identifier =
    t.Identifier(
      id match {
        case IdentifierExpr.Named(name) => t.Identifier.Value.Named(name)
        case IdentifierExpr.OperatorIdentifier(op) => t.Identifier.Value.Operator(op.symbol)
        case IdentifierExpr.Extension(inner) => t.Identifier.Value.Extension(getIdentifier(inner))
        case IdentifierExpr.Inverse(inner) => t.Identifier.Value.Inverse(getIdentifier(inner))
        case IdentifierExpr.Update(inner) => t.Identifier.Value.Update(getIdentifier(inner))
        case IdentifierExpr.FunctionResultValue => t.Identifier.Value.FunctionResultValue(com.google.protobuf.empty.Empty())
      }
    )

  def getMutability(mutability: Boolean): t.Mutability =
    if mutability then
      t.Mutability.Mutable
    else
      t.Mutability.NonMutable

  def getAccessModifier(access: AccessModifier): t.AccessModifier =
    access match {
      case AccessModifier.Public => t.AccessModifier.Public
      case AccessModifier.TubePrivate => t.AccessModifier.TubePrivate
      case AccessModifier.ModulePrivate => t.AccessModifier.ModulePrivate
      case AccessModifier.TubeOrProtected => t.AccessModifier.TubeOrProtected
      case AccessModifier.TubeAndProtected => t.AccessModifier.TubeAndProtected
      case AccessModifier.Protected => t.AccessModifier.Protected
      case AccessModifier.Private => t.AccessModifier.Private
    }

  trait LocalVariableContext {
    def getVarId(local: LocalVariable): UIO[BigInt]
  }

  def getExprWithVariables(expr: WrapExpr): Comp[t.Expr] =
    withVariables {
      getExpr(expr)
    }

  def withVariables[A](f: LocalVariableContext ?=> Comp[A]): Comp[A] =
    TMap.empty[LocalVariable, BigInt].commit.flatMap { varMap =>
      given LocalVariableContext with
        override def getVarId(local: context.ExprContext.LocalVariable): UIO[BigInt] =
          varMap.get(local).flatMap {
            case Some(id) => ZSTM.succeed(id)
            case None =>
              varMap.size
                .map(id => id : BigInt)
                .tap(varMap.put(local, _))
          }.commit
      end given

      f
    }

  def getVarId(local: LocalVariable)(using varContext: LocalVariableContext): UIO[BigInt] =
    varContext.getVarId(local)

  def getVariableDeclaration(variable: LocalVariable)(using LocalVariableContext): Comp[t.LocalVariableDefinition] =
    for
      id <- getVarId(variable)
      varType <- getExpr(variable.varType)
    yield t.LocalVariableDefinition(
      id = id,
      flags = getFlags(
        t.LocalVariableDefinition.Flags.IsErased -> variable.isErased,
      ),
      varType = varType,
      name = variable.name.map(getIdentifier),
      isMutable = variable.isMutable,
    )

  def getExpr(expr: WrapExpr)(using LocalVariableContext): Comp[t.Expr] =

    def getParameterVariableOwner(owner: ParameterVariableOwner): Comp[t.ParameterVariableOwner] =
      (
        owner match {
          case owner: ArClass => getIdOf[ArClass](owner).map(t.ParameterVariableOwner.Owner.ClassId.apply)
          case owner: ArTrait => getIdOf[ArTrait](owner).map(t.ParameterVariableOwner.Owner.TraitId.apply)
          case owner: ArMethod => getIdOf[ArMethod](owner).map(t.ParameterVariableOwner.Owner.MethodId.apply)
          case owner: ArFunc => getIdOf[ArFunc](owner).map(t.ParameterVariableOwner.Owner.FunctionId.apply)
          case owner: ClassConstructor => getIdOf[ClassConstructor](owner).map(t.ParameterVariableOwner.Owner.ClassConstructorId.apply)
        }
      ).map(t.ParameterVariableOwner.apply)

    def getVariableReference(variable: Variable): Comp[t.VariableReference] =
      variable match {
        case variable: context.ExprContext.LocalVariable =>
          for
            id <- getVarId(variable)
          yield t.LocalVariableReference(id)

        case variable: context.ExprContext.InstanceVariable =>
          for
            id <- getIdOf[ArMethod](variable.method)
          yield t.InstanceVariableReference(
            methodId = id,
          )

        case variable: context.ExprContext.MemberVariable =>
          for
            id <- getIdOf[ArClass](variable.ownerClass)
          yield t.MemberVariableReference(
            classId = id,
            name = getIdentifier(variable.name.get),
          )

        case variable: context.ExprContext.ParameterVariable =>
          for
            owner <- getParameterVariableOwner(variable.owner)
          yield t.ParameterVariableReference(
            owner = owner,
            parameterIndex = variable.parameterIndex,
          )

        case variable: context.ExprContext.FunctionResultVariable =>
          for
            owner <- getParameterVariableOwner(variable.owner)
          yield t.FunctionResultVariableReference(
            owner = owner,
          )
      }

    def getExprCtor(ctor: ExprConstructor): Comp[t.Expr.Constructor] =
      ctor match
        case ExprConstructor.BindVariable(variable) =>
          for
            decl <- getVariableDeclaration(variable)
          yield t.Expr.Constructor.BindVariable(decl)

        case ExprConstructor.ClassConstructorCall(classCtor) =>
          for
            id <- getIdOf[ClassConstructor](classCtor)
          yield t.Expr.Constructor.ClassConstructorCall(t.Expr.LoadId(id))

        case ExprConstructor.EnsureExecuted =>
          ZIO.succeed(t.Expr.Constructor.EnsureExecuted(Empty()))

        case ExprConstructor.FunctionCall(function) =>
          for
            id <- getIdOf[ArFunc](function)
          yield t.Expr.Constructor.FunctionCall(t.Expr.LoadId(id))

        case ExprConstructor.FunctionObjectCall =>
          ZIO.succeed(t.Expr.Constructor.FunctionObjectCall(Empty()))

        case ExprConstructor.IfElse(whenTrue, whenFalse) =>
          for
            whenTrueVar <- ZIO.foreach(whenTrue)(getVariableDeclaration)
            whenFalseVar <- ZIO.foreach(whenFalse)(getVariableDeclaration)
          yield t.Expr.Constructor.IfElse(t.Expr.IfElse(whenTrueVar, whenFalseVar))

        case ExprConstructor.LoadConstantBool(b) =>
          ZIO.succeed(t.Expr.Constructor.LoadConstantBool(b))

        case ExprConstructor.LoadConstantInt(i) =>
          ZIO.succeed(t.Expr.Constructor.LoadConstantInt(i))

        case ExprConstructor.LoadConstantString(s) =>
          ZIO.succeed(t.Expr.Constructor.LoadConstantString(s))

        case ExprConstructor.LoadLambda(argVariable) =>
          for
            decl <- getVariableDeclaration(argVariable)
          yield t.Expr.Constructor.LoadLambda(t.Expr.LoadLambda(decl))

        case ExprConstructor.LoadTuple =>
          ZIO.succeed(t.Expr.Constructor.LoadTuple(Empty()))

        case ExprConstructor.LoadTupleElement(index) =>
          ZIO.succeed(t.Expr.Constructor.LoadTupleElement(index))

        case ExprConstructor.LoadVariable(variable) =>
          for
            ref <- getVariableReference(variable)
          yield t.Expr.Constructor.LoadVariable(ref)

        case ExprConstructor.MethodCall(method) =>
          for
            id <- getIdOf[ArMethod](method)
          yield t.Expr.Constructor.MethodCall(t.Expr.LoadId(id))

        case ExprConstructor.PatternMatch(patterns) => ???

        case ExprConstructor.Proving(witnesses) =>
          for
            witnesses <- ZIO.foreach(witnesses)(getVariableDeclaration)
          yield t.Expr.Constructor.Proving(t.Expr.WitnessList(witnesses))

        case ExprConstructor.RaiseException =>
          ZIO.succeed(t.Expr.Constructor.RaiseException(Empty()))

        case ExprConstructor.Sequence =>
          ZIO.succeed(t.Expr.Constructor.Sequence(Empty()))

        case ExprConstructor.StoreVariable(variable) =>
          for
            ref <- getVariableReference(variable)
          yield t.Expr.Constructor.StoreVariable(ref)

        case ExprConstructor.TypeN =>
          ZIO.succeed(t.Expr.Constructor.TypeN(Empty()))

        case ExprConstructor.OmegaTypeN(level) =>
          ZIO.succeed(t.Expr.Constructor.OmegaTypeN(level))

        case ExprConstructor.AnyType =>
          ZIO.succeed(t.Expr.Constructor.AnyType(Empty()))

        case ExprConstructor.TraitType(arTrait) =>
          for
            id <- getIdOf[ArTrait](arTrait)
          yield t.Expr.Constructor.TraitType(t.Expr.LoadId(id))

        case ExprConstructor.ClassType(arClass) =>
          for
            id <- getIdOf[ArClass](arClass)
          yield t.Expr.Constructor.ClassType(t.Expr.LoadId(id))

        case ExprConstructor.FunctionType =>
          ZIO.succeed(t.Expr.Constructor.FunctionType(Empty()))

        case ExprConstructor.UnionType =>
          ZIO.succeed(t.Expr.Constructor.UnionType(Empty()))

        case ExprConstructor.IntersectionType =>
          ZIO.succeed(t.Expr.Constructor.IntersectionType(Empty()))

        case ExprConstructor.ExistentialType(variable) =>
          for
            decl <- getVariableDeclaration(variable)
          yield t.Expr.Constructor.ExistentialType(t.Expr.ExistentialType(decl))

        case ExprConstructor.ConjunctionType =>
          ZIO.succeed(t.Expr.Constructor.ConjunctionType(Empty()))

        case ExprConstructor.DisjunctionType =>
          ZIO.succeed(t.Expr.Constructor.DisjunctionType(Empty()))

        case ExprConstructor.NeverType =>
          ZIO.succeed(t.Expr.Constructor.NeverType(Empty()))

        case ExprConstructor.SubtypeWitnessType =>
          ZIO.succeed(t.Expr.Constructor.SubtypeWitnessType(Empty()))

        case ExprConstructor.EqualTo =>
          ZIO.succeed(t.Expr.Constructor.EqualToType(Empty()))

        case ExprConstructor.AssumeErasedValue =>
          ZIO.succeed(t.Expr.Constructor.AssumeErasedValue(Empty()))
      end match

    expr match {
      case WrapExpr.OfExpr(expr) =>
        for
          ctor <- getExprCtor(expr.constructor)
          args <- ZIO.foreach(expr.constructor.argsToExprs(expr.args))(getExpr)
        yield t.Expr(
          constructor = ctor,
          arguments = args,
        )

      case WrapExpr.OfHole(hole) => hole
    }
  end getExpr

  private def emitClassType(classType: ArExpr[ExprConstructor.ClassType]): Comp[t.ClassType] =
    withVariables {
      for
        id <- getIdOf[ArClass](classType.constructor.arClass)
        args <- ZIO.foreach(classType.args)(getExpr)
      yield t.ClassType(
        classId = id,
        arguments = args,
      )
    }

  private def emitTraitType(traitType: ArExpr[ExprConstructor.TraitType]): Comp[t.TraitType] =
    withVariables {
      for
        id <- getIdOf[ArTrait](traitType.constructor.arTrait)
        args <- ZIO.foreach(traitType.args)(getExpr)
      yield t.TraitType(
        traitId = id,
        arguments = args,
      )
    }


}
