package dev.argon.plugins.tube

import dev.argon.compiler.{definitions, *}
import dev.argon.compiler.definitions.*
import dev.argon.compiler.module.ModuleName
import dev.argon.compiler.signature.Signature
import dev.argon.io.{BinaryResource, DirectoryEntry, DirectoryResource, ProtobufResource, ResourceRecorder, ZipFileResource}
import dev.argon.parser.IdentifierExpr
import dev.argon.tube as t
import dev.argon.util.{*, given}
import zio.*
import zio.stm.*
import scalapb.{GeneratedEnum, GeneratedMessage}
import com.google.protobuf.empty.Empty
import dev.argon.compiler.module.ModuleElementC
import dev.argon.options.OptionCodec
import dev.argon.util.toml.Toml

import java.io.IOException

trait TubeWriterBase extends UsingContext {


  val context: Context { type Error >: TubeError }
  type IsImplementation <: Boolean

  protected def ifImplementation[A, B, C](value: IsImplementation match {
    case true => A
    case false => B
  })(whenImplementation: A => C, whenInterface: B => C): C

  protected def dummyImplementationValue: IsImplementation match {
    case true => Unit
    case false => Unit
  }

  private def processImplementation[Impl, T](impl: IsImplementation match {
    case true => Comp[Impl]
    case false => Unit
  })(f: Impl => Comp[T]): Comp[Option[T]] =
    ifImplementation(impl)(
      whenImplementation = _.flatMap(f).asSome,
      whenInterface = _ => ZIO.none,
    )

  val currentTube: ArTube & HasImplementation[IsImplementation]


  import context.ExprContext.{
    WrapExpr,
    ArExpr,
    ExprConstructor,
    Variable,
    LocalVariable,
    ParameterVariableOwner,
    ClassResult,
    TraitResult,
    FunctionResult,
  }

  private def getIdOf[A](ids: TMap[A, BigInt])(a: A): UIO[BigInt] =
    ids.get(a).flatMap {
      case Some(value) => ZSTM.succeed(value)
      case None =>
        ids.size.map { _ + 1 : BigInt }
          .tap(ids.put(a, _))
    }.commit

  protected def pushEntry(entry: ZipFileResource.Entry[context.Env, context.Error]): UIO[Unit]

  private def pushEntryMessage(zipPath: String)(message: GeneratedMessage): UIO[Unit] =
    pushEntry(new ZipFileResource.Entry[context.Env, context.Error] {
      override val path: String = zipPath

      override def value: BinaryResource[context.Env, context.Error] =
        new ProtobufResource[context.Env, context.Error, GeneratedMessage] with ProtobufResource.Impl[context.Env, context.Error, GeneratedMessage] {
          override def fileName: Option[String] = None

          override def asMessage: ZIO[context.Env, context.Error, GeneratedMessage] =
            ZIO.succeed(message)
        }
    })


  def emitTube(options: IsImplementation match {
    case true => context.Options
    case false => Unit
  })(using optionsCodec: OptionCodec[context.Env, context.Error, context.Options]): Comp[Unit] =
    for
      _ <- pushEntryMessage(Paths.get[t.TubeFormatVersion])(
        t.TubeProto.defaultTubeFormatVersion.get(t.TubeFormatVersion.scalaDescriptor.getOptions).get
      )

      _ <- ZIO.foreachDiscard(currentTube.modulePaths) { path =>
        currentTube.module(path).flatMap { module =>
          emitModule(module)
        }
      }

      resourceRecorder <- zipResourceRecorder

      options <- ifImplementation(options)(
        whenImplementation = options => optionsCodec.encode(resourceRecorder)(options).map(encodeToml).asSome,
        whenInterface = _ => ZIO.none,
      )

      metadata <- buildMetadata(t.Metadata(
        name = t.TubeName(currentTube.tubeName.name.toList),
        `type` = ifImplementation(dummyImplementationValue)(
          whenImplementation = _ => t.TubeType.Implementation,
          whenInterface = _ => t.TubeType.Interface
        ),
        options = options,
        modules = currentTube.modulePaths
          .iterator
          .map { modulePath => t.ModulePath(modulePath.ids) }
          .toSeq,

        references = Seq.empty,
        externalClasses = Seq.empty,
        externalTraits = Seq.empty,
        externalFunctions = Seq.empty,
        externalMethods = Seq.empty,
        externalClassConstructors = Seq.empty,
      ))
      _ <- pushEntryMessage(Paths.get[t.Metadata])(
        ???
      )

    yield ()

  private def zipResourceRecorder: UIO[ResourceRecorder[context.Env, context.Error]] =
    for
      nextId <- Ref.make[BigInt](1)
    yield new ResourceRecorder[context.Env, context.Error] {
      private def getNextPath: UIO[String] =
        nextId.getAndUpdate(_ + 1).map { id =>
          Paths.get[t.Resource].format(id)
        }

      override def recordBinaryResource(resource: BinaryResource[context.Env, context.Error]): ZIO[context.Env, context.Error, String] =
        for
          newPath <- getNextPath
          _ <- pushEntry(new ZipFileResource.Entry[context.Env, context.Error] {
            override val path: String = newPath
            override def value: BinaryResource[context.Env, context.Error] = resource
          })
        yield newPath


      private def recordDirWithPrefix(prefix: String)(resource: DirectoryResource[context.Env, context.Error, BinaryResource]): Comp[Unit] =
        resource.contents.foreach {
          case DirectoryEntry.Subdirectory(name, resource) => recordDirWithPrefix(prefix + "/" + name)(resource)
          case DirectoryEntry.File(name, resource) =>
            pushEntry(new ZipFileResource.Entry[context.Env, context.Error] {
              override val path: String = prefix + "/" + name
              override def value: BinaryResource[context.Env, context.Error] = resource
            })
        }

      override def recordDirectoryResource(resource: DirectoryResource[context.Env, context.Error, BinaryResource]): ZIO[context.Env, context.Error, String] =
        for
          newPath <- getNextPath
          _ <- recordDirWithPrefix(newPath)(resource)
        yield newPath




    }

  private def encodeToml(toml: Toml): t.Toml =
    toml match {
      case Toml.Table(map) =>
        val values = map
          .iterator
          .map { (k, v) => t.TomlKeyValue(k, encodeToml(v)) }
          .toSeq

        t.Toml(t.Toml.Value.Table(t.TomlTable(values)))

      case Toml.Array(value) =>
        t.Toml(t.Toml.Value.Array(t.TomlArray(value.map(encodeToml))))

      case Toml.String(value) =>
        t.Toml(t.Toml.Value.StringValue(value))

      case Toml.Int(value) =>
        t.Toml(t.Toml.Value.IntValue(value))

      case Toml.Float(value) =>
        t.Toml(t.Toml.Value.FloatValue(value))

      case Toml.Boolean(value) =>
        t.Toml(t.Toml.Value.BoolValue(value))

      case Toml.OffsetDateTime(value) =>
        t.Toml(t.Toml.Value.OffsetDateTime(value))

      case Toml.LocalDateTime(value) =>
        t.Toml(t.Toml.Value.LocalDateTime(value))

      case Toml.LocalDate(value) =>
        t.Toml(t.Toml.Value.LocalDate(value))

      case Toml.LocalTime(value) =>
        t.Toml(t.Toml.Value.LocalTime(value))
    }

  private def emitModule(module: ArModule & HasImplementation[IsImplementation]): Comp[Unit] =
    for
      exports <- module.allExports(Set.empty)
      elements <- ZIO.foreach(exports.groupBy(_.name).toSeq) { (name, exports) =>
        val (exported, decl) = exports.partitionMap {
          case ModuleElementC.ClassElement(arClass) =>
            Right(
              for
                id <- emitClass(arClass)
              yield t.ModuleDefinition.ElementDeclaration(
                id = id,
                `type` = t.ModuleDefinition.ElementDeclaration.Type.Class,
                accessModifier = getAccessModifier(arClass.owner.accessModifier),
              )
            )

          case ModuleElementC.TraitElement(arTrait) =>
            Right(
              for
                id <- emitTrait(arTrait)
              yield t.ModuleDefinition.ElementDeclaration(
                id = id,
                `type` = t.ModuleDefinition.ElementDeclaration.Type.Trait,
                accessModifier = getAccessModifier(arTrait.owner.accessModifier),
              )
            )

          case ModuleElementC.FunctionElement(func) =>
            Right(
              for
                id <- emitFunction(func)
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

      modulePath = Paths.get[t.ModuleDefinition] + (
        if module.moduleName.path.ids.isEmpty then
          ""
        else
          "/" + module.moduleName.path.urlEncode
      )


      _ <- pushEntryMessage(modulePath)(t.ModuleDefinition(
        elements = elements,
      ))

    yield ()


  private def buildMetadata(metadata: t.Metadata): Comp[t.Metadata] =
    ???



  private def getFlags[T, Enum <: GeneratedEnum](flags: (Enum, Boolean)*): Int =
    flags.foldLeft(0) { case (acc, (enumValue, flagValue)) =>
      acc | (if flagValue then enumValue.value else 0)
    }


  private def emitSignature[Res, A](sig: Signature[WrapExpr, Res])(f: (Seq[t.Expr], Res) => Comp[A]): Comp[A] =
    def impl(sig: Signature[WrapExpr, Res])(paramTypes: Seq[t.Expr]): Comp[A] =
      sig match {
        case Signature.Parameter(_, _, _, paramType, next) =>
          getExprWithVariables(paramType).flatMap { t =>
            impl(next)(paramTypes :+ t)
          }

        case Signature.Result(resultType) =>
          f(paramTypes, resultType)
      }

    impl(sig)(Seq.empty)

  private def getModuleName(moduleName: ModuleName): t.ModuleName =
    t.ModuleName(
      tube = moduleName.tubeName.name.toList,
      module = moduleName.path.ids,
    )


  val classIds: TMap[ArClass, BigInt]

  def getClassId(arClass: ArClass): Comp[BigInt] =
    getIdOf(classIds)(arClass)

  private def emitClass(arClass: ArClass & HasImplementation[IsImplementation]): Comp[BigInt] =
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
          id <- emitClassConstructor(ctor)
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

  private def emitTrait(arTrait: ArTrait & HasImplementation[IsImplementation]): Comp[BigInt] =
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

  def emitFunction(func: ArFunc & HasImplementation[IsImplementation]): Comp[BigInt] =
    for
      _ <- func.validate

      flags = getFlags(
        t.FunctionDefinition.Flags.Erased -> func.isErased,
        t.FunctionDefinition.Flags.Proof -> func.isProof,
      )

      sig <- func.signature
      signature <- emitSignature(sig)(emitFunctionSignature)

      body <- processImplementation(func.implementation) {
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

  def emitFunctionSignature(paramTypes: Seq[t.Expr], functionResult: FunctionResult): Comp[t.FunctionSignature] =
    for
      returnType <- getExprWithVariables(functionResult.returnType)
      ensuresClauses <- ZIO.foreach(functionResult.ensuresClauses)(getExprWithVariables)
    yield t.FunctionSignature(
      parameterTypes = paramTypes,
      resultType = returnType,
      ensuresClauses = ensuresClauses,
    )


  def createMethodMemberGroup(name: Option[IdentifierExpr], methods: Seq[ArMethod & HasImplementation[IsImplementation]]): Comp[Seq[t.MethodMember]] =
    ZIO.foreach(methods)(createMethodMember(name))

  def createMethodMember(name: Option[IdentifierExpr])(method: ArMethod & HasImplementation[IsImplementation]): Comp[t.MethodMember] =
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

  def emitMethod(method: ArMethod & HasImplementation[IsImplementation]): Comp[BigInt] =
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

      body <- processImplementation(method.implementation) {
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
        body = body.flatten,
      ))
    yield id



  val classCtorIds: TMap[ClassConstructor, BigInt]

  def getClassConstructorId(classCtor: ClassConstructor): Comp[BigInt] =
    getIdOf(classCtorIds)(classCtor)

  def emitClassConstructor(classCtor: ClassConstructor & HasImplementation[IsImplementation]): Comp[BigInt] =
    for
      sig <- classCtor.signatureUnsubstituted
      signature <- emitSignature(sig) { (paramTypes, _) =>
        ZIO.succeed(t.ClassConstructorDefinition.Signature(paramTypes))
      }

      body <- processImplementation(classCtor.implementation) {
        case impl: ClassConstructorImplementationC.ExpressionBody =>
          withVariables { getVarId =>
            def getPreInit(stmt: Either[WrapExpr, ClassConstructorImplementationC.FieldInitializationStatement & HasContext[context.type]]): Comp[t.ClassConstructorDefinition.PreInitializationStatement] =
              stmt match {
                case Left(expr) =>
                  for
                    expr <- getExpr(getVarId)(expr)
                  yield t.ClassConstructorDefinition.PreInitializationStatement(
                    t.ClassConstructorDefinition.PreInitializationStatement.Value.Expr(expr)
                  )
                case Right(fieldInit) =>
                  for
                    value <- getExpr(getVarId)(fieldInit.value)
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
                getExpr(getVarId)(WrapExpr.OfExpr(call))
              }
              instanceVar <- getVarId(impl.baseConstructorCall.instanceVariable)
              postInit <- getExpr(getVarId)(impl.postInitialization)
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

  def getExprWithVariables(expr: WrapExpr): Comp[t.Expr] =
    withVariables { getVarId =>
      getExpr(getVarId)(expr)
    }

  def withVariables[A](f: (LocalVariable => UIO[BigInt]) => Comp[A]): Comp[A] =
    TMap.empty[LocalVariable, BigInt].commit.flatMap { varMap =>
      f(getIdOf(varMap))
    }

  def getExpr(getVarId: LocalVariable => UIO[BigInt])(expr: WrapExpr): Comp[t.Expr] =
    def getVariableDeclaration(variable: LocalVariable): Comp[t.LocalVariableDefinition] =
      for
        id <- getVarId(variable)
        varType <- getExpr(getVarId)(variable.varType)
      yield t.LocalVariableDefinition(
        flags = getFlags(
          t.LocalVariableDefinition.Flags.IsErased -> variable.isErased,
        ),
        varType = varType,
        name = variable.name.map(getIdentifier),
        isMutable = variable.isMutable,
      )

    def getParameterVariableOwner(owner: ParameterVariableOwner): Comp[t.ParameterVariableOwner] =
      (
        owner match {
          case owner: ArClass => getClassId(owner).map(t.ParameterVariableOwner.Owner.ClassId.apply)
          case owner: ArTrait => getTraitId(owner).map(t.ParameterVariableOwner.Owner.TraitId.apply)
          case owner: ArMethod => getMethodId(owner).map(t.ParameterVariableOwner.Owner.MethodId.apply)
          case owner: ArFunc => getFunctionId(owner).map(t.ParameterVariableOwner.Owner.FunctionId.apply)
          case owner: ClassConstructor => getClassConstructorId(owner).map(t.ParameterVariableOwner.Owner.ClassConstructorId.apply)
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
            id <- getMethodId(variable.method)
          yield t.InstanceVariableReference(
            methodId = id,
          )

        case variable: context.ExprContext.MemberVariable =>
          for
            id <- getClassId(variable.ownerClass)
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
            id <- getClassConstructorId(classCtor)
          yield t.Expr.Constructor.ClassConstructorCall(t.Expr.LoadId(id))

        case ExprConstructor.EnsureExecuted =>
          ZIO.succeed(t.Expr.Constructor.EnsureExecuted(Empty()))

        case ExprConstructor.FunctionCall(function) =>
          for
            id <- getFunctionId(function)
          yield t.Expr.Constructor.FunctionCall(t.Expr.LoadId(id))

        case ExprConstructor.FunctionObjectCall =>
          ZIO.succeed(t.Expr.Constructor.FunctionObjectCall(Empty()))

        case ExprConstructor.IfElse =>
          ZIO.succeed(t.Expr.Constructor.IfElse(Empty()))

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
            id <- getMethodId(method)
          yield t.Expr.Constructor.MethodCall(t.Expr.LoadId(id))

        case ExprConstructor.PatternMatch(patterns) => ???
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
            id <- getTraitId(arTrait)
          yield t.Expr.Constructor.TraitType(t.Expr.LoadId(id))

        case ExprConstructor.ClassType(arClass) =>
          for
            id <- getClassId(arClass)
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
          args <- ZIO.foreach(expr.constructor.argsToExprs(expr.args))(getExpr(getVarId))
        yield t.Expr(
          constructor = ctor,
          arguments = args,
        )

      case WrapExpr.OfHole(hole) => hole
    }
  end getExpr

  private def emitClassType(classType: ArExpr[ExprConstructor.ClassType]): Comp[t.ClassType] =
    withVariables { getVarId =>
      for
        id <- getClassId(classType.constructor.arClass)
        args <- ZIO.foreach(classType.args)(getExpr(getVarId))
      yield t.ClassType(
        classId = id,
        arguments = args,
      )
    }

  private def emitTraitType(traitType: ArExpr[ExprConstructor.TraitType]): Comp[t.TraitType] =
    withVariables { getVarId =>
      for
        id <- getTraitId(traitType.constructor.arTrait)
        args <- ZIO.foreach(traitType.args)(getExpr(getVarId))
      yield t.TraitType(
        traitId = id,
        arguments = args,
      )
    }


}
