package dev.argon.plugins.tube

import dev.argon.compiler.{definitions, *}
import dev.argon.compiler.definitions.*
import dev.argon.compiler.signature.Signature
import dev.argon.io.ZipFileResource
import dev.argon.parser.IdentifierExpr
import dev.argon.tube as t
import zio.*
import zio.stm.*
import scalapb.{GeneratedEnum, GeneratedMessage}

trait TubeWriter extends UsingContext {


  val context: Context { type Error >: TubeError }

  val emitTube: ArTube


  import context.ExprContext.{
    WrapExpr,
    ArExpr,
    ExprConstructor,
    ClassResult,
    TraitResult,
    FunctionResult,
  }

  private def getIdOf[A](ids: TMap[A, BigInt])(a: A): Comp[BigInt] =
    ids.get(a).flatMap {
      case Some(value) => ZSTM.succeed(value)
      case None =>
        ids.size.map { _ + 1 : BigInt }
          .tap(ids.put(a, _))
    }.commit

  def pushEntry(entry: ZipFileResource.Entry[context.Env, context.Error]): Comp[Unit] = ???

  def pushEntryMessage(path: String)(message: GeneratedMessage): Comp[Unit] = ???


  private def getFlags[T, Enum <: GeneratedEnum](flags: (Enum, Boolean)*): Int =
    flags.foldLeft(0) { case (acc, (enumValue, flagValue)) =>
      acc | (if flagValue then enumValue.value else 0)
    }


  private def emitSignature[Res, A](sig: Signature[WrapExpr, Res])(f: (Seq[t.ExprWithVariables], Res) => Comp[A]): Comp[A] =
    def impl(sig: Signature[WrapExpr, Res])(paramTypes: Seq[t.ExprWithVariables]): Comp[A] =
      sig match {
        case Signature.Parameter(_, _, _, paramType, next) =>
          getExprWithVariables(paramType).flatMap { t =>
            impl(next)(paramTypes :+ t)
          }

        case Signature.Result(resultType) =>
          f(paramTypes, resultType)
      }

    impl(sig)(Seq.empty)

  private def emitClassType(classType: ArExpr[ExprConstructor.ClassType]): Comp[t.ClassType] =
    ???

  private def emitTraitType(traitType: ArExpr[ExprConstructor.TraitType]): Comp[t.TraitType] =
    ???


  val classIds: TMap[ArClass, BigInt]

  def getClassId(arClass: ArClass): Comp[BigInt] =
    getIdOf(classIds)(arClass)

  private def emitClass(arClass: ArClass & HasImplementation[true]): Comp[BigInt] =
    for
      _ <- arClass.validate

      flags = getFlags(
        t.ClassDefinition.Flags.Abstract -> arClass.isAbstract,
        t.ClassDefinition.Flags.Sealed -> arClass.isSealed,
        t.ClassDefinition.Flags.Open -> arClass.isOpen,
      )

      sig <- arClass.signature
      signature <- emitSignature(sig) { case (paramTypes, ClassResult(_, baseClass, baseTraits)) =>
        for
          baseClass <- baseClass
          baseClass <- ZIO.foreach(baseClass)(emitClassType)

          baseTraits <- baseTraits
          baseTraits <- ZIO.foreach(baseTraits)(emitTraitType)
        yield t.ClassDefinition.Signature(
          parameterTypes = paramTypes,
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
          id <- getClassConstructorId(ctor)
        yield t.ClassConstructorMember(
          id,
          getAccessModifier(ctor.owner.accessModifier),
        )
      }

      id <- getClassId(arClass)
      _ <- pushEntryMessage(Paths.get[t.ClassDefinition].format(id))(t.ClassDefinition(
        flags = flags,
        signature = signature,
        fields = fields,
        methods = methods.flatten,
        staticMethods = staticMethods.flatten,
        constructors = constructors,
      ))

    yield id


  val traitIds: TMap[ArTrait, BigInt]

  def getTraitId(arTrait: ArTrait): Comp[BigInt] =
    getIdOf(traitIds)(arTrait)

  private def emitTrait(arTrait: ArTrait & HasImplementation[true]): Comp[BigInt] =
    for
      _ <- arTrait.validate

      flags = getFlags(
        t.TraitDefinition.Flags.Sealed -> arTrait.isSealed,
      )

      sig <- arTrait.signature
      signature <- emitSignature(sig) { case (paramTypes, TraitResult(_, baseTraits)) =>
        for
          baseTraits <- baseTraits
          baseTraits <- ZIO.foreach(baseTraits)(emitTraitType)
        yield t.TraitDefinition.Signature(
          parameterTypes = paramTypes,
          baseTraits = baseTraits,
        )
      }

      methods <- arTrait.methods
      methods <- ZIO.foreach(methods.toSeq)(createMethodMemberGroup)

      staticMethods <- arTrait.staticMethods
      staticMethods <- ZIO.foreach(staticMethods.toSeq)(createMethodMemberGroup)

      id <- getTraitId(arTrait)
      _ <- pushEntryMessage(Paths.get[t.TraitDefinition].format(id))(t.TraitDefinition(
        flags = flags,
        signature = signature,
        methods = methods.flatten,
        staticMethods = staticMethods.flatten,
      ))

    yield id


  val functionIds: TMap[ArFunc, BigInt]

  def getFunctionId(func: ArFunc): Comp[BigInt] =
    getIdOf(functionIds)(func)

  def emitFunction(func: ArFunc & HasImplementation[true]): Comp[BigInt] =
    for
      _ <- func.validate

      flags = getFlags(
        t.FunctionDefinition.Flags.Erased -> func.isErased,
        t.FunctionDefinition.Flags.Proof -> func.isProof,
      )

      sig <- func.signature
      signature <- emitSignature(sig)(emitFunctionSignature)

      impl <- func.implementation
      body <- impl match {
        case impl: FunctionImplementationC.ExpressionBody =>
          for
            body <- getExprWithVariables(impl.body)
          yield t.FunctionBody(t.FunctionBody.Value.ExpressionBody(body))

        case impl: FunctionImplementationC.External =>
          ZIO.succeed(t.FunctionBody(t.FunctionBody.Value.ExternalImplementation(impl.name)))
      }


      id <- getFunctionId(func)
      _ <- pushEntryMessage(Paths.get[t.FunctionDefinition].format(id))(t.FunctionDefinition(
        flags = flags,
        signature = signature,
        effects = t.EffectInfo(
          isPure = func.purity,
        ),
        body = body,
      ))
    yield id

  def emitFunctionSignature(paramTypes: Seq[t.ExprWithVariables], functionResult: FunctionResult): Comp[t.FunctionSignature] =
    for
      returnType <- getExprWithVariables(functionResult.returnType)
      ensuresClauses <- ZIO.foreach(functionResult.ensuresClauses)(getExprWithVariables)
    yield t.FunctionSignature(
      parameterTypes = paramTypes,
      resultType = returnType,
      ensuresClauses = ensuresClauses,
    )


  def createMethodMemberGroup(name: Option[IdentifierExpr], methods: Seq[ArMethod & HasImplementation[true]]): Comp[Seq[t.MethodMember]] =
    ZIO.foreach(methods)(createMethodMember(name))

  def createMethodMember(name: Option[IdentifierExpr])(method: ArMethod & HasImplementation[true]): Comp[t.MethodMember] =
    for
      id <- emitMethod(method)
    yield t.MethodMember(
      id = id,
      name = name.map(getIdentifier),
      accessModifier = getAccessModifier(ArMethodC.getAccessModifier(method.owner))
    )


  val methodIds: TMap[ArMethod, BigInt]

  def getMethodId(method: ArMethod): Comp[BigInt] =
    getIdOf(methodIds)(method)

  def emitMethod(method: ArMethod & HasImplementation[true]): Comp[BigInt] =
    for
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

      impl <- method.implementation
      body <- impl match {
        case impl: MethodImplementationC.ExpressionBody =>
          for
            body <- getExprWithVariables(impl.body)
          yield Some(t.FunctionBody(t.FunctionBody.Value.ExpressionBody(body)))

        case impl: MethodImplementationC.External =>
          ZIO.succeed(Some(t.FunctionBody(t.FunctionBody.Value.ExternalImplementation(impl.name))))

        case impl: MethodImplementationC.Abstract =>
          ZIO.succeed(None)
      }

      id <- getMethodId(method)
      _ <- pushEntryMessage(Paths.get[t.MethodDefinition].format(id))(t.MethodDefinition(
        flags = flags,
        signature = signature,
        effects = t.EffectInfo(
          isPure = method.purity,
        ),
        body = body,
      ))
    yield id



  val classCtorIds: TMap[ClassConstructor, BigInt]

  def getClassConstructorId(classCtor: ClassConstructor): Comp[BigInt] =
    getIdOf(classCtorIds)(classCtor)

  def emitClassConstructor(classCtor: ClassConstructor & HasImplementation[true]): Comp[BigInt] =
    for
      sig <- classCtor.signatureUnsubstituted
      signature <- emitSignature(sig) { (paramTypes, _) =>
        ZIO.succeed(t.ClassConstructorDefinition.Signature(paramTypes))
      }

      impl <- classCtor.implementation
      body <- impl match {
        case impl: ClassConstructorImplementationC.ExpressionBody =>
          ???

        case impl: ClassConstructorImplementationC.External =>
          ZIO.succeed(t.ClassConstructorDefinition.Body(t.ClassConstructorDefinition.Body.Value.ExternalImplementation(impl.name)))
      }

      id <- getClassConstructorId(classCtor)
      _ <- pushEntryMessage(Paths.get[t.ClassConstructorDefinition].format(id))(t.ClassConstructorDefinition(
        flags = 0,
        signature = signature,
        effects = t.EffectInfo(
          isPure = classCtor.purity,
        ),
        body = body,
      ))
    yield id

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

  def getExprWithVariables(expr: WrapExpr): Comp[t.ExprWithVariables] = ???

}
