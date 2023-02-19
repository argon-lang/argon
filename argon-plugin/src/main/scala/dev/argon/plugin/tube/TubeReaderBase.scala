package dev.argon.plugin.tube

import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.VariableUtil
import dev.argon.compiler.module.*
import dev.argon.compiler.signature.Signature
import dev.argon.compiler.tube.*
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.compiler.*
import dev.argon.io.*
import dev.argon.parser.{FunctionParameterListType, IdentifierExpr}
import dev.argon.{parser, tube as t}
import dev.argon.util.protobuf.given
import dev.argon.util.toml.*
import dev.argon.util.{*, given}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.*
import zio.stm.*
import zio.stream.*


abstract class TubeReaderBase extends UsingContext {
  val context: Context { type Error >: InvalidTube }
  protected val serialized: SerializedTube[context.Env, context.Error]
  protected val tubeImporter: TubeImporter & HasContext[context.type]
  protected val vtableBuilder: VTableBuilder[context.type]

  type IsImplementation <: Boolean


  import context.ExprContext.{ArExpr, ClassResult, ExprConstructor, FunctionResult, FunctionResultVariable, InstanceVariable, LocalVariable, MemberVariable, ParameterVariable, ParameterVariableOwner, TraitResult, Variable, WrapExpr}

  protected def withHasImplementationImpl[A]
  (tube: ArTube & HasImplementation[IsImplementation])
  (
    whenImplementation: tube.type & HasImplementation[true] => A,
    whenInterface: tube.type & HasImplementation[false] => A,
  ): A

  protected def ensureOptions(opt: Option[context.Options]): Comp[IsImplementation match {
    case true => context.Options
    case false => Unit
  }]
  protected def createImplementation[TImpl](currentTube: ArTube & HasImplementation[IsImplementation])(f: Option[context.Options] => Comp[Option[TImpl]]): IsImplementation match {
    case true => Comp[TImpl]
    case false => Comp[Option[TImpl]]
  }
  protected def getMaybeImplementation[TImpl]: (IsImplementation match {
    case true => Comp[TImpl]
    case false => Comp[Option[TImpl]]
  }) => Comp[Option[TImpl]]

  private def decodeToml(toml: t.Toml): Comp[Toml] =
    ZIO.fromOption(t.TomlConverter.decodeToml(toml))
      .mapError { _ => InvalidTube("Unknown TOML value") }

  trait ReferenceLoader {
    def getClass(id: BigInt): Comp[ArClass]
    def getTrait(id: BigInt): Comp[ArTrait]
    def getFunction(id: BigInt): Comp[ArFunc]
    def getMethod(id: BigInt): Comp[ArMethod]
    def getClassConstructor(id: BigInt): Comp[ClassConstructor]

    def putClass(id: BigInt, definition: ArClass): Comp[Unit]
    def putTrait(id: BigInt, definition: ArTrait): Comp[Unit]
    def putFunction(id: BigInt, definition: ArFunc): Comp[Unit]
    def putMethod(id: BigInt, definition: ArMethod): Comp[Unit]
    def putClassConstructor(id: BigInt, definition: ClassConstructor): Comp[Unit]
  }

  private def getIdentifier(id: t.Identifier): Comp[IdentifierExpr] =
    id.value match {
      case t.Identifier.Value.Named(name) =>
        ZIO.succeed(IdentifierExpr.Named(name))

      case t.Identifier.Value.Operator(opSymbol) =>
        ZIO.fromEither(
          summon[TypeNameLookup[parser.Operator]]
            .asMap
            .valuesIterator
            .find { op => op.symbol == opSymbol }
            .toRight {
              InvalidTube(s"Unknown symbol: $opSymbol")
            }
            .map(IdentifierExpr.OperatorIdentifier.apply)
        )

      case t.Identifier.Value.Extension(inner) =>
        getIdentifier(inner).map(IdentifierExpr.Extension.apply)

      case t.Identifier.Value.Inverse(inner) =>
        getIdentifier(inner).map(IdentifierExpr.Inverse.apply)

      case t.Identifier.Value.Update(inner) =>
        getIdentifier(inner).map(IdentifierExpr.Update.apply)

      case t.Identifier.Value.FunctionResultValue(_) =>
        ZIO.succeed(IdentifierExpr.FunctionResultValue)

      case _: t.Identifier.Value.Empty.type =>
        ZIO.fail(InvalidTube("Unknown identifier type"))

    }

  private def getModuleName(name: t.ModuleName): Comp[ModuleName] =
    ZIO.fromEither(
      NonEmptyList.fromList(name.tube.toList).toRight(InvalidTube("Tube name must be non-empty"))
    ).map { tubeName =>
      ModuleName(
        TubeName(tubeName),
        ModulePath(name.module),
      )
    }


  private def getAccessModifier(access: t.AccessModifier): Comp[AccessModifier] =
    access match {
      case t.AccessModifier.Private => ZIO.succeed(AccessModifier.Private)
      case t.AccessModifier.TubePrivate => ZIO.succeed(AccessModifier.TubePrivate)
      case t.AccessModifier.ModulePrivate => ZIO.succeed(AccessModifier.ModulePrivate)
      case t.AccessModifier.Public => ZIO.succeed(AccessModifier.Public)
      case t.AccessModifier.Protected => ZIO.succeed(AccessModifier.Protected)
      case t.AccessModifier.TubeOrProtected => ZIO.succeed(AccessModifier.TubeOrProtected)
      case t.AccessModifier.TubeAndProtected => ZIO.succeed(AccessModifier.TubeAndProtected)
      case t.AccessModifier.Unrecognized(_) => ZIO.fail(InvalidTube("Invalid access modifier"))
    }

  private def getAccessModifierGlobal(access: t.AccessModifier): Comp[AccessModifierGlobal] =
    getAccessModifier(access).flatMap {
      case access: AccessModifierGlobal => ZIO.succeed(access)
      case _ => ZIO.fail(InvalidTube("Expected global access modifier"))
    }



  private def createReferenceLoader(metadata: t.Metadata): Comp[ReferenceLoader] =
    for
      classIds <- TMap.empty[BigInt, ArClass].commit
      traitIds <- TMap.empty[BigInt, ArTrait].commit
      functionIds <- TMap.empty[BigInt, ArFunc].commit
      methodIds <- TMap.empty[BigInt, ArMethod].commit
      classCtorIds <- TMap.empty[BigInt, ClassConstructor].commit
    yield new ReferenceLoader {
      private def getId[T](idMap: TMap[BigInt, T], id: BigInt): Comp[T] =
        idMap.get(id).commit.flatMap {
          case Some(value) => ZIO.succeed(value)
          case None => ZIO.fail(InvalidTube("Could not find requested id"))
        }

      override def getClass(id: BigInt): Comp[ArClass] =
        getId(classIds, id)

      override def getTrait(id: BigInt): Comp[ArTrait] =
        getId(traitIds, id)

      override def getFunction(id: BigInt): Comp[ArFunc] =
        getId(functionIds, id)

      override def getMethod(id: BigInt): Comp[ArMethod] =
        getId(methodIds, id)

      override def getClassConstructor(id: BigInt): Comp[ClassConstructor] =
        getId(classCtorIds, id)

      private def putId[T](idMap: TMap[BigInt, T], id: BigInt, definition: T): Comp[Unit] =
        STM.ifSTM(idMap.contains(id))(
          onTrue = STM.fail(InvalidTube("Duplicate ID found")),
          onFalse = idMap.put(id, definition)
        ).commit

      override def putClass(id: BigInt, definition: ArClass): Comp[Unit] =
        putId(classIds, id, definition)

      override def putTrait(id: BigInt, definition: ArTrait): Comp[Unit] =
        putId(traitIds, id, definition)

      override def putFunction(id: BigInt, definition: ArFunc): Comp[Unit] =
        putId(functionIds, id, definition)

      override def putMethod(id: BigInt, definition: ArMethod): Comp[Unit] =
        putId(methodIds, id, definition)

      override def putClassConstructor(id: BigInt, definition: ClassConstructor): Comp[Unit] =
        putId(classCtorIds, id, definition)
    }


  private object SerializedResourceFactory extends ResourceFactory[context.Env, context.Error] {
    override def directoryResource(name: String): DirectoryResource[context.Env, context.Error, BinaryResource] = ???

    override def binaryResource(name: String): BinaryResource[context.Env, context.Error] = ???
  }


  def asTube: Comp[ArTube & HasImplementation[IsImplementation]] =
    for
      metadata <- serialized.metadata
      tubeNameChecked <- ZIO.fromEither(
        NonEmptyList.fromList(metadata.name.name.toList)
          .toRight { InvalidTube("Tube name must be non-empty") }
      )

      decodedOptions <- ZIO.foreach(metadata.options)(decodeToml)
      decodedOptions <- ZIO.foreach(decodedOptions)(context.optionsCodec.decode(SerializedResourceFactory))
        .mapError { error => InvalidTube(s"Error parsing tube options: $error") }
      decodedOptions <- ensureOptions(decodedOptions)

      moduleCache <- MemoCacheStore.make[context.Env, context.Error, ModulePath, ArModule & HasImplementation[IsImplementation]]

      given ReferenceLoader <- createReferenceLoader(metadata)

    yield new ArTubeC {
      override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
      override type IsImplementation = TubeReaderBase.this.IsImplementation

      override val tubeName: TubeName = TubeName(tubeNameChecked)

      override def asDeclaration: Option[this.type & HasImplementation[true]] =
        withHasImplementation(Some.apply, _ => None)

      override def withHasImplementation[A]
      (
        whenImplementation: this.type & HasImplementation[true] => A,
        whenInterface: this.type & HasImplementation[false] => A,
      ): A = withHasImplementationImpl(this)(whenImplementation, whenInterface)

      override val options: IsImplementation match {
        case true => context.Options
        case false => Unit
      } = decodedOptions

      override val modulePaths: Set[ModulePath] =
        metadata.modules.iterator.map(decodeModulePath).toSet

      override def module(path: ModulePath): Comp[ArModule & HasImplementation[IsImplementation]] =
        moduleCache.usingCreate(path)(loadModule(this))

      private def decodeModulePath(path: t.ModulePath): ModulePath =
        ModulePath(path.name)


    }


  private trait EnsureModuleLoaded {
    def ensureLoaded: Comp[Unit]
  }


  private def loadModule(currentTube: ArTube & HasImplementation[IsImplementation])(path: ModulePath)(using ReferenceLoader): Comp[ArModule & HasImplementation[IsImplementation]] =
    type LoadExported = Set[ArModule] => Comp[Seq[ModuleElement[IsImplementation]]]
    type PartialElements = Map[Option[IdentifierExpr], (Seq[ModuleElement[IsImplementation]], LoadExported)]
    for
      moduleElementsCache <- MemoCacheStore.make[context.Env, context.Error, Option[IdentifierExpr], Seq[ModuleElement[IsImplementation]]]
      partialElementsCache <- MemoCell.make[context.Env, context.Error, PartialElements]

      modName = ModuleName(currentTube.tubeName, path)
      _ <- ZIO.fail(DiagnosticError.UnknownModuleException(modName)).unless(currentTube.modulePaths.contains(path))

      moduleInfo <- serialized.getModule(t.ModulePath(path.ids))

      module = new ArModuleC with EnsureModuleLoaded {
        override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
        override type IsImplementation = TubeReaderBase.this.IsImplementation
        override val tube: ArTube & HasImplementation[IsImplementation] = currentTube

        override val moduleName: ModuleName = modName

        def ensureLoaded: Comp[Unit] =
          getPartialElements.unit

        override def allExports(exportingModules: Set[ArModule]): Comp[Seq[ModuleElement[TubeReaderBase.this.IsImplementation]]] =
          for
            partialElements <- getPartialElements
            res <- ZIO.foreach(partialElements.keys)(exports(exportingModules))
          yield res.flatten.toSeq

        override def exports(exportingModules: Set[ArModule])(name: Option[IdentifierExpr]): Comp[Seq[ModuleElement[IsImplementation]]] =
          moduleElementsCache.usingCreate(name) { _ =>
            getPartialElements.flatMap { partialElements =>
              partialElements.get(name) match
                case Some((elements, getExportedElements)) =>
                  for
                    exportedElements <- getExportedElements(exportingModules)
                  yield elements ++ exportedElements

                case None =>
                  ZIO.succeed(Seq.empty)
              end match
            }
          }

        private def getPartialElements: Comp[PartialElements] =
          partialElementsCache.get(
            ZIO.foreach(moduleInfo.elements) { nameGroup =>
              for
                name <- ZIO.foreach(nameGroup.name)(getIdentifier)
                res <- loadNameGroup(name)(nameGroup)
              yield name -> res
            }.map {
              _.toMap
            }
          )

        def loadNameGroup(name: Option[IdentifierExpr])(nameGroup: t.ModuleDefinition.NameGroup): Comp[(Seq[ModuleElement[IsImplementation]], LoadExported)] =
          for
            decl <- ZIO.foreach(nameGroup.declaredElements)(loadElementDecl(name))
          yield (decl, loadExportedElements(nameGroup.exportedElements))

        def loadElementDecl(name: Option[IdentifierExpr])(element: t.ModuleDefinition.ElementDeclaration): Comp[ModuleElement[IsImplementation]] =
          for
            access <- getAccessModifierGlobal(element.accessModifier)
            ownership = OwnedByModuleC[context.type, IsImplementation](this, name, access)
            elem <- element.`type` match {
              case t.ModuleDefinition.ElementDeclaration.Type.Class =>
                loadClass(ownership, element.id)
                  .map(ModuleElementC.ClassElement.apply)

              case t.ModuleDefinition.ElementDeclaration.Type.Trait =>
                loadTrait(ownership, element.id)
                  .map(ModuleElementC.TraitElement.apply)

              case t.ModuleDefinition.ElementDeclaration.Type.Function =>
                loadFunction(ownership, element.id)
                  .map(ModuleElementC.FunctionElement.apply)

              case t.ModuleDefinition.ElementDeclaration.Type.Unknown | t.ModuleDefinition.ElementDeclaration.Type.Unrecognized(_) =>
                ZIO.fail(InvalidTube("Invalid element declaration"))
            }
          yield elem

        def loadExportedElements(elements: Seq[t.ModuleDefinition.ElementExported]): LoadExported = exportingModules =>
          ZIO.foreach(elements)(loadElementExported(exportingModules))
            .map { _.flatten }

        def loadElementExported(exportingModules: Set[ArModule])(element: t.ModuleDefinition.ElementExported): Comp[Seq[ModuleElement[IsImplementation]]] =
          for
            moduleName <- getModuleName(element.module)
            tube <- tubeImporter.getTube(moduleName.tubeName)
            module <- tube.module(moduleName.path): Comp[ArModule]
            name <- ZIO.foreach(element.name)(getIdentifier)
            exports <- module.exports(exportingModules)(name)
          yield exports.map { elem => ModuleElementC.ExportedElement(module, name, elem) }

      }

      _ <- module.ensureLoaded

    yield module
  end loadModule

  private def loadParameter(param: t.Parameter)(using ReferenceLoader): Comp[[TResult] => Signature[WrapExpr, TResult] => Signature[WrapExpr, TResult]] =
    for
      listType <- param.listType match {
        case t.Parameter.ListType.NormalList => ZIO.succeed(FunctionParameterListType.NormalList)
        case t.Parameter.ListType.InferrableList => ZIO.succeed(FunctionParameterListType.InferrableList)
        case t.Parameter.ListType.InferrableList2 => ZIO.succeed(FunctionParameterListType.InferrableList2)
        case t.Parameter.ListType.RequiresList => ZIO.succeed(FunctionParameterListType.RequiresList)
        case t.Parameter.ListType.Unrecognized(_) => ZIO.fail(InvalidTube("Unknown parameter list type"))
      }

      name <- ZIO.foreach(param.name)(getIdentifier)
      paramType <- loadExprWithVariables(param.paramType)

    yield [TResult] => (next: Signature[WrapExpr, TResult]) => Signature.Parameter(
      paramListType = listType,
      isErased = (param.flags & t.Parameter.Flags.Erased.value) == t.Parameter.Flags.Erased.value,
      name = name,
      paramType = paramType,
      next = next,
    )

  private def loadClass(classOwnership: ArClassC.Ownership[context.type, IsImplementation], id: BigInt)(using refLoader: ReferenceLoader): Comp[ArClass & HasOwner[classOwnership.type] & HasImplementation[IsImplementation]] =
    for
      classDef <- serialized.getClass(id)

      id2 <- UniqueIdentifier.make

      sigCell <- MemoCell.make[context.Env, context.Error, Signature[WrapExpr, ClassResult]]
      methodsCell <- MemoCell.make[context.Env, context.Error, Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByClass[classOwnership.type, IsImplementation]]]]]
      staticMethodsCell <- MemoCell.make[context.Env, context.Error, Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByClassStatic[classOwnership.type, IsImplementation]]]]]
      constructorsCell <- MemoCell.make[context.Env, context.Error, Seq[ClassConstructor & HasImplementation[IsImplementation]]]
      fieldsCell <- MemoCell.make[context.Env, context.Error, Seq[MemberVariable]]

      arClass = new ArClassC {
        override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
        override type IsImplementation = TubeReaderBase.this.IsImplementation
        override val owner: classOwnership.type = classOwnership
        override val id: UniqueIdentifier = id2

        override def isAbstract: Boolean = (classDef.flags & t.ClassDefinition.Flags.Abstract.value) == t.ClassDefinition.Flags.Abstract.value
        override def isSealed: Boolean = (classDef.flags & t.ClassDefinition.Flags.Sealed.value) == t.ClassDefinition.Flags.Sealed.value
        override def isOpen: Boolean = (classDef.flags & t.ClassDefinition.Flags.Open.value) == t.ClassDefinition.Flags.Open.value

        override def classMessageSource: DiagnosticSource =
          DiagnosticSource.SerializedTube(ArClassC.getOwningTube(owner).tubeName)

        override def signature: Comp[Signature[WrapExpr, ClassResult]] =
          sigCell.get(
            for
              params <- ZIO.foreach(classDef.signature.parameters)(loadParameter)
              classSuperType <- loadExprWithVariables(classDef.signature.classTypeSuperType)

              result = Signature.Result(ClassResult(
                classSuperType,
                ZIO.foreach(classDef.signature.baseClass)(loadClassType),
                ZIO.foreach(classDef.signature.baseTraits)(loadTraitType),
              )) : Signature[WrapExpr, ClassResult]
            yield
              params.foldLeft(result) { (next, param) =>
                param(next)
              }
          )

        override def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByClass[owner.type, IsImplementation]]]]] =
          methodsCell.get(
            loadMethodGroups[OwnedByClass[owner.type, IsImplementation]](
              OwnedByClassC[context.type, owner.type, IsImplementation](this, _, _),
              classDef.methods
            )
          )

        override def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByClassStatic[owner.type, IsImplementation]]]]] =
          staticMethodsCell.get(
            loadMethodGroups[OwnedByClassStatic[owner.type, IsImplementation]](
              OwnedByClassStaticC[context.type, owner.type, IsImplementation](this, _, _),
              classDef.staticMethods
            )
          )

        override def constructors: Comp[Seq[ClassConstructor & HasImplementation[IsImplementation]]] =
          constructorsCell.get(
            ZIO.foreach(classDef.constructors) { ctor =>
              for
                access <- getAccessModifier(ctor.accessModifier)
                ctor <- loadClassConstructor(OwnedByClassC(this, None, access), ctor.id)
              yield ctor
            }
          )

        override def fields: Comp[Seq[MemberVariable]] =
          fieldsCell.get(
            ZIO.foreach(classDef.fields) { field =>
              for
                name <- getIdentifier(field.name)
                fieldType <- loadExprWithVariables(field.fieldType)
                isMutable <- field.mutability match {
                  case t.Mutability.Mutable => ZIO.succeed(true)
                  case t.Mutability.NonMutable => ZIO.succeed(false)
                  case t.Mutability.Unrecognized(_) => ZIO.fail(InvalidTube("Invalid mutability specifier"))
                }
              yield MemberVariable(
                this,
                varType = fieldType,
                name = Some(name),
                isMutable = isMutable,
              )
            }
          )

        override def vtable: Comp[context.VT.VTable] =
          vtableBuilder.fromClass(this)

        override def vtableDiff: Comp[context.VT.VTable] =
          vtableBuilder.diffFromClass(this)

        override def validate: Comp[Unit] = ZIO.unit
      }

      _ <- refLoader.putClass(id, arClass)
    yield arClass

  private def loadTrait(traitOwnership: ArTraitC.Ownership[context.type, IsImplementation], id: BigInt)(using refLoader: ReferenceLoader): Comp[ArTrait & HasOwner[traitOwnership.type] & HasImplementation[IsImplementation]] =
    for
      traitDef <- serialized.getTrait(id)

      id2 <- UniqueIdentifier.make

      sigCell <- MemoCell.make[context.Env, context.Error, Signature[WrapExpr, TraitResult]]
      methodsCell <- MemoCell.make[context.Env, context.Error, Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByTrait[traitOwnership.type, IsImplementation]]]]]
      staticMethodsCell <- MemoCell.make[context.Env, context.Error, Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByTraitStatic[traitOwnership.type, IsImplementation]]]]]

      arTrait = new ArTraitC {
        override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
        override type IsImplementation = TubeReaderBase.this.IsImplementation
        override val owner: traitOwnership.type = traitOwnership
        override val id: UniqueIdentifier = id2

        override def isSealed: Boolean = (traitDef.flags & t.TraitDefinition.Flags.Sealed.value) == t.TraitDefinition.Flags.Sealed.value

        override def signature: Comp[Signature[WrapExpr, TraitResult]] =
          sigCell.get(
            for
              params <- ZIO.foreach(traitDef.signature.parameters)(loadParameter)
              traitSuperType <- loadExprWithVariables(traitDef.signature.traitTypeSuperType)

              result = Signature.Result(TraitResult(
                traitSuperType,
                ZIO.foreach(traitDef.signature.baseTraits)(loadTraitType),
              )): Signature[WrapExpr, TraitResult]
            yield
              params.foldLeft(result) { (next, param) =>
                param(next)
              }
          )

        override def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByTrait[owner.type, IsImplementation]]]]] =
          methodsCell.get(
            loadMethodGroups[OwnedByTrait[owner.type, IsImplementation]](
              OwnedByTraitC[context.type, owner.type, IsImplementation](this, _, _),
              traitDef.methods
            )
          )

        override def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByTraitStatic[owner.type, IsImplementation]]]]] =
          staticMethodsCell.get(
            loadMethodGroups[OwnedByTraitStatic[owner.type, IsImplementation]](
              OwnedByTraitStaticC[context.type, owner.type, IsImplementation](this, _, _),
              traitDef.staticMethods
            )
          )

        override def vtable: Comp[context.VT.VTable] =
          vtableBuilder.fromTrait(this)

        override def validate: Comp[Unit] = ZIO.unit
      }

      _ <- refLoader.putTrait(id, arTrait)
    yield arTrait

  private def loadFunction(funcOwnership: ArFuncC.Ownership[context.type, IsImplementation], id: BigInt)(using refLoader: ReferenceLoader): Comp[ArFunc & HasOwner[funcOwnership.type] & HasImplementation[IsImplementation]] =
    for
      funcDef <- serialized.getFunction(id)

      id2 <- UniqueIdentifier.make

      sigCell <- MemoCell.make[context.Env, context.Error, Signature[WrapExpr, FunctionResult]]
      implCell <- MemoCell.make[context.Env, context.Error, Option[FunctionImplementation]]

      func = new ArFuncC {
        override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
        override type IsImplementation = TubeReaderBase.this.IsImplementation
        override val owner: funcOwnership.type = funcOwnership
        override val id: UniqueIdentifier = id2


        override def purity: Boolean = funcDef.effects.isPure
        override def isProof: Boolean = (funcDef.flags & t.FunctionDefinition.Flags.Proof.value) == t.FunctionDefinition.Flags.Proof.value
        override def isErased: Boolean = (funcDef.flags & t.FunctionDefinition.Flags.Erased.value) == t.FunctionDefinition.Flags.Erased.value
        override def isInline: Boolean = (funcDef.flags & t.FunctionDefinition.Flags.Inline.value) == t.FunctionDefinition.Flags.Inline.value

        override def signature: Comp[Signature[WrapExpr, FunctionResult]] =
          sigCell.get(
            for
              params <- ZIO.foreach(funcDef.signature.parameters)(loadParameter)
              returnType <- loadExprWithVariables(funcDef.signature.returnType)
              ensuresClauses <- ZIO.foreach(funcDef.signature.ensuresClauses)(loadExprWithVariables)

              result = Signature.Result(FunctionResult(
                returnType = returnType,
                ensuresClauses = ensuresClauses,
              )): Signature[WrapExpr, FunctionResult]
            yield
              params.foldLeft(result) { (next, param) =>
                param(next)
              }
          )

        override def implementation: ImplementationType =
          createImplementation(ArFuncC.getOwningTube(owner)) { options =>
            funcDef.body match {
              case Some(body) =>
                implCell.get(
                  body.value match {
                    case t.FunctionBody.Value.ExpressionBody(body) =>
                      for
                        bodyExpr <- loadExprWithVariables(body)
                      yield Some(new FunctionImplementationC.ExpressionBody {
                        override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                        override val body: WrapExpr = bodyExpr
                      })

                    case t.FunctionBody.Value.ExternalImplementation(specifier) =>
                      ZIO.foreach(options) { options =>
                        context.getExternFunctionImplementation(options, specifier)
                          .mapBoth(
                            {
                              case Some(e) => e
                              case None => DiagnosticError.ExternFunctionNotFound(DiagnosticSource.SerializedTube(ArFuncC.getOwningTube(owner).tubeName), specifier)
                            },
                            extern => new FunctionImplementationC.External {
                              override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                              override val name: String = specifier
                              override val impl: context.ExternFunctionImplementation = extern
                            }
                          )
                      }

                    case _: t.FunctionBody.Value.Empty.type =>
                      ZIO.fail(InvalidTube("Unknown function body"))
                  }
                )
              case None =>
                ZIO.none
            }

          }

        override def maybeImplementation: Comp[Option[FunctionImplementation]] =
          getMaybeImplementation(implementation)


        override def validate: Comp[Unit] = ZIO.unit
      }

      _ <- refLoader.putFunction(id, func)
    yield func

  private def loadMethodGroups[TMethodOwner <: ArMethodC.Ownership[context.type, IsImplementation]]
  (
    ownership: (Option[IdentifierExpr], AccessModifier) => TMethodOwner,
    methodGroups: Seq[t.MethodMemberGroup],
  )
  (using ReferenceLoader)
  : Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[TMethodOwner]]]] =
    ZIO.foreach(methodGroups) { methodGroup =>
      for
        name <- ZIO.foreach(methodGroup.name)(getIdentifier)
        methods <- ZIO.foreach(methodGroup.methods) { methodMember =>
          for
            access <- getAccessModifier(methodMember.accessModifier)
            method <- loadMethod(ownership(name, access), methodMember.id)
          yield method
        }
      yield name -> methods
    }.map { _.toMap }

  private def loadMethod(methodOwnership: ArMethodC.Ownership[context.type, IsImplementation], id: BigInt)(using refLoader: ReferenceLoader): Comp[ArMethod & HasOwner[methodOwnership.type] & HasImplementation[IsImplementation]] =
    for
      methodDef <- serialized.getMethod(id)

      id2 <- UniqueIdentifier.make

      sigCell <- MemoCell.make[context.Env, context.Error, Signature[WrapExpr, FunctionResult]]
      implCell <- MemoCell.make[context.Env, context.Error, Option[MethodImplementation]]

      instanceVarName <- ZIO.foreach(methodDef.instanceVariableName)(getIdentifier)

      method = new ArMethodC {
        override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
        override type IsImplementation = TubeReaderBase.this.IsImplementation
        override val owner: methodOwnership.type = methodOwnership
        override val id: UniqueIdentifier = id2


        override def purity: Boolean = methodDef.effects.isPure


        override def isAbstract: Boolean = (methodDef.flags & t.MethodDefinition.Flags.Abstract.value) == t.MethodDefinition.Flags.Abstract.value
        override def isImplicitOverride: Boolean = (methodDef.flags & t.MethodDefinition.Flags.AutoOverride.value) == t.MethodDefinition.Flags.AutoOverride.value
        override def isVirtual: Boolean = (methodDef.flags & t.MethodDefinition.Flags.Virtual.value) == t.MethodDefinition.Flags.Virtual.value || isAbstract || isImplicitOverride
        override def isFinal: Boolean = (methodDef.flags & t.MethodDefinition.Flags.Final.value) == t.MethodDefinition.Flags.Final.value
        override def isErased: Boolean = (methodDef.flags & t.MethodDefinition.Flags.Erased.value) == t.MethodDefinition.Flags.Erased.value
        override def isProof: Boolean = (methodDef.flags & t.MethodDefinition.Flags.Proof.value) == t.MethodDefinition.Flags.Proof.value
        override def isInline: Boolean = (methodDef.flags & t.MethodDefinition.Flags.Inline.value) == t.MethodDefinition.Flags.Inline.value

        override def instanceVariableName: Option[IdentifierExpr] = instanceVarName

        override def signatureUnsubstituted: Comp[Signature[WrapExpr, FunctionResult]] =
          sigCell.get(
            for
              params <- ZIO.foreach(methodDef.signature.parameters)(loadParameter)
              returnType <- loadExprWithVariables(methodDef.signature.returnType)
              ensuresClauses <- ZIO.foreach(methodDef.signature.ensuresClauses)(loadExprWithVariables)

              result = Signature.Result(FunctionResult(
                returnType = returnType,
                ensuresClauses = ensuresClauses,
              )): Signature[WrapExpr, FunctionResult]
            yield
              params.foldLeft(result) { (next, param) =>
                param(next)
              }
          )

        override def implementation: ImplementationType =
          createImplementation(ArMethodC.getOwningTube(owner)) { options =>
            implCell.get(
              methodDef.body match {
                case Some(body) =>
                  body.value match {
                    case t.FunctionBody.Value.ExpressionBody(body) =>
                      for
                        bodyExpr <- loadExprWithVariables(body)
                      yield Some(new MethodImplementationC.ExpressionBody {
                        override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                        override val body: WrapExpr = bodyExpr
                      })

                    case t.FunctionBody.Value.ExternalImplementation(specifier) =>
                      ZIO.foreach(options) { options =>
                        context.getExternMethodImplementation(options, specifier)
                          .mapBoth(
                            {
                              case Some(e) => e
                              case None => DiagnosticError.ExternMethodNotFound(DiagnosticSource.SerializedTube(ArMethodC.getOwningTube(owner).tubeName), specifier)
                            },
                            extern => new MethodImplementationC.External {
                              override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                              override val name: String = specifier
                              override val impl: context.ExternMethodImplementation = extern
                            }
                          )
                      }

                    case _: t.FunctionBody.Value.Empty.type =>
                      ZIO.fail(InvalidTube("Unknown function body"))
                  }

                case None if isAbstract && options.isDefined =>
                  ZIO.some(new MethodImplementationC.Abstract {
                    override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                  })

                case None =>
                  ZIO.none
              }
            )
          }

        override def maybeImplementation: Comp[Option[MethodImplementation]] =
          getMaybeImplementation(implementation)

        override def validate: Comp[Unit] = ZIO.unit
      }

      _ <- refLoader.putMethod(id, method)
    yield method

  private def loadClassConstructor(ctorOwnership: ClassConstructorC.Ownership[context.type, IsImplementation], id: BigInt)(using refLoader: ReferenceLoader): Comp[ClassConstructor & HasOwner[ctorOwnership.type] & HasImplementation[IsImplementation]] =
    for
      ctorDef <- serialized.getClassConstructor(id)

      id2 <- UniqueIdentifier.make

      sigCell <- MemoCell.make[context.Env, context.Error, Signature[WrapExpr, Unit]]
      implCell <- MemoCell.make[context.Env, context.Error, Option[ClassConstructorImplementation]]

      ctor = new ClassConstructorC {
        override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
        override type IsImplementation = TubeReaderBase.this.IsImplementation
        override val owner: ctorOwnership.type = ctorOwnership
        override val id: UniqueIdentifier = id2


        override def purity: Boolean = ctorDef.effects.isPure

        override def signatureUnsubstituted: Comp[Signature[WrapExpr, Unit]] =
          sigCell.get(
            for
              params <- ZIO.foreach(ctorDef.signature.parameters)(loadParameter)
              result = Signature.Result(()): Signature[WrapExpr, Unit]
            yield
              params.foldLeft(result) { (next, param) =>
                param(next)
              }
          )

        override def implementation: ImplementationType =
          createImplementation(ClassConstructorC.getOwningTube(owner)) { options =>
            implCell.get(
              ctorDef.body match {
                case Some(body) =>
                  withVariables {
                    body.value match {
                      case t.ClassConstructorDefinition.Body.Value.ExpressionBody(body) =>
                        for
                          preInit <- ZIO.foreach(body.preInitialization) {
                            case t.ClassConstructorDefinition.PreInitializationStatement(t.ClassConstructorDefinition.PreInitializationStatement.Value.Expr(expr)) =>
                              loadExprWithVariables(expr).asLeft

                            case t.ClassConstructorDefinition.PreInitializationStatement(t.ClassConstructorDefinition.PreInitializationStatement.Value.FieldInit(fieldInit)) =>
                              for
                                fieldName <- getIdentifier(fieldInit.field)
                                fields <- owner.arClass.fields
                                field2 <- ZIO.fromEither(
                                  fields.find(_.name.get == fieldName)
                                    .toRight {
                                      InvalidTube("Invalid field")
                                    }
                                )
                                value2 <- loadExprWithVariables(fieldInit.value)
                              yield Right(new ClassConstructorImplementationC.FieldInitializationStatement {
                                override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                                override val field: MemberVariable = field2
                                override val value: WrapExpr = value2
                              })

                            case t.ClassConstructorDefinition.PreInitializationStatement(_: t.ClassConstructorDefinition.PreInitializationStatement.Value.Empty.type) =>
                              ZIO.fail(InvalidTube("Missing class constructor pre-init statement"))
                          }

                          instanceVar <- loadLocalVariableDefinition(body.instanceVariable)

                          baseCallExpr <- ZIO.foreach(body.baseConstructorCall) { baseCallExpr =>
                            for
                              baseCallExpr <- loadExpr(baseCallExpr)
                              baseCallExpr <- baseCallExpr match {
                                case WrapExpr.OfExpr(expr) =>
                                  expr.constructor match {
                                    case ctor: (expr.constructor.type & ExprConstructor.ClassConstructorCall) =>
                                      ZIO.succeed(ArExpr(ctor, expr.args))

                                    case _ =>
                                      ZIO.fail(InvalidTube("Invalid constructor call"))
                                  }

                                case WrapExpr.OfHole(hole) => hole
                              }
                            yield baseCallExpr
                          }

                          postInit <- loadExpr(body.postInitialization)
                        yield Some(new ClassConstructorImplementationC.ExpressionBody {
                          override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                          override val preInitialization: Seq[Either[WrapExpr, ClassConstructorImplementationC.FieldInitializationStatement & HasContext[context.type]]] =
                            preInit

                          override val baseConstructorCall: ClassConstructorImplementationC.BaseClassConstructorCallStatement & HasContext[context.type] =
                            new ClassConstructorImplementationC.BaseClassConstructorCallStatement {
                              override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                              override val baseCall: Option[ArExpr[ExprConstructor.ClassConstructorCall]] =
                                baseCallExpr

                              override val instanceVariable: LocalVariable = instanceVar
                            }


                          override val postInitialization: WrapExpr =
                            postInit
                        })

                      case t.ClassConstructorDefinition.Body.Value.ExternalImplementation(specifier) =>
                        ZIO.foreach(options) { options =>
                          context.getExternClassConstructorImplementation(options, specifier)
                            .mapBoth(
                              {
                                case Some(e) => e
                                case None => DiagnosticError.ExternMethodNotFound(DiagnosticSource.SerializedTube(ClassConstructorC.getOwningTube(owner).tubeName), specifier)
                              },
                              extern => new ClassConstructorImplementationC.External {
                                override val context: TubeReaderBase.this.context.type = TubeReaderBase.this.context
                                override val name: String = specifier
                                override val impl: context.ExternClassConstructorImplementation = extern
                              }
                            )
                        }

                      case _: t.ClassConstructorDefinition.Body.Value.Empty.type =>
                        ZIO.fail(InvalidTube("Unknown class constructor body"))
                    }
                  }

                case None =>
                  ZIO.none
              }
            )
          }
      }

      _ <- refLoader.putClassConstructor(id, ctor)
    yield ctor


  trait LocalVariableContext {
    def declare(id: BigInt, variable: LocalVariable): Comp[Unit]
    def get(id: BigInt): Comp[LocalVariable]
  }

  private def withVariables[A](f: LocalVariableContext ?=> Comp[A]): Comp[A] =
    TMap.empty[BigInt, LocalVariable].commit.flatMap { varMap =>
      given LocalVariableContext with
        override def declare(id: BigInt, variable: LocalVariable): Comp[Unit] =
          STM.ifSTM(varMap.contains(id))(
            onTrue = varMap.put(id, variable),
            onFalse = STM.fail(InvalidTube("Local variable with same ID declared multiple times")),
          ).commit

        override def get(id: BigInt): Comp[context.ExprContext.LocalVariable] =
          varMap.get(id).commit.flatMap { local =>
            ZIO.fromEither(local.toRight { InvalidTube("Unknown local variable") })
          }
      end given

      f
    }

  private def loadExprWithVariables(expr: t.Expr)(using ReferenceLoader): Comp[WrapExpr] =
    withVariables(loadExpr(expr))

  private def getLocalVariable(id: BigInt)(using varContext: LocalVariableContext): Comp[LocalVariable] =
    varContext.get(id)

  private def getParameterVariableOwner(owner: t.ParameterVariableOwner)(using refLoader: ReferenceLoader): Comp[ParameterVariableOwner] =
    owner.owner match {
      case t.ParameterVariableOwner.Owner.MethodId(id) =>
        refLoader.getMethod(id)

      case t.ParameterVariableOwner.Owner.FunctionId(id) =>
        refLoader.getFunction(id)

      case t.ParameterVariableOwner.Owner.ClassId(id) =>
        refLoader.getClass(id)

      case t.ParameterVariableOwner.Owner.TraitId(id) =>
        refLoader.getTrait(id)

      case t.ParameterVariableOwner.Owner.ClassConstructorId(id) =>
        refLoader.getClassConstructor(id)

      case _: t.ParameterVariableOwner.Owner.Empty.type =>
        ZIO.fail(InvalidTube("Invalid parameter variable owner"))
    }

  private def getVariable(variable: t.VariableReference)(using refLoader: ReferenceLoader, varContext: LocalVariableContext): Comp[Variable] =
    variable match {
      case variable: t.LocalVariableReference => getLocalVariable(variable.id)
      case variable: t.InstanceVariableReference =>
        for
          method <- refLoader.getMethod(variable.methodId)
          instanceVar <- VariableUtil.getInstanceVariable(context)(method)
        yield instanceVar

      case variable: t.MemberVariableReference =>
        for
          name <- getIdentifier(variable.name)
          arClass <- refLoader.getClass(variable.classId)
          fields <- arClass.fields : Comp[Seq[MemberVariable]]
          field <- ZIO.fromEither(
            fields.find(_.name.get == name)
              .toRight { InvalidTube("Unknown field") }
          )
        yield field

      case variable: t.ParameterVariableReference =>
        for
          owner <- getParameterVariableOwner(variable.owner)
          paramVar <- VariableUtil.getParameterVariable(context)(owner)(variable.parameterIndex)
          paramVar <- ZIO.fromEither(paramVar.toRight { InvalidTube("Invalid parameter variable") })
        yield paramVar

      case variable: t.FunctionResultVariableReference =>
        for
          owner <- getParameterVariableOwner(variable.owner)
          resVar <- VariableUtil.getFunctionResultVariable(context)(owner)
          resVar <- ZIO.fromEither(resVar.toRight { InvalidTube("Invalid function result variable") })
        yield resVar

      case _: t.VariableReference.Empty.type =>
        ZIO.fail(InvalidTube("Unknown type of variable"))
    }

  private def loadLocalVariableDefinition(varDef: t.LocalVariableDefinition)(using ReferenceLoader, LocalVariableContext): Comp[LocalVariable] =
    for
      id <- UniqueIdentifier.make
      varType <- loadExpr(varDef.varType)
      name <- ZIO.foreach(varDef.name)(getIdentifier)
    yield LocalVariable(
      id = id,
      varType = varType,
      name = name,
      isMutable = varDef.isMutable,
      isErased = (varDef.flags & t.LocalVariableDefinition.Flags.IsErased.value) == t.LocalVariableDefinition.Flags.IsErased.value
    )

  private def loadClassType(expr: t.ClassType)(using refLoader: ReferenceLoader): Comp[ArExpr[ExprConstructor.ClassType]] =
    for
      arClass <- refLoader.getClass(expr.classId)
      args <- ZIO.foreach(expr.arguments)(loadExprWithVariables)
    yield ArExpr(
      ExprConstructor.ClassType(arClass),
      args,
    )

  private def loadTraitType(expr: t.TraitType)(using refLoader: ReferenceLoader): Comp[ArExpr[ExprConstructor.TraitType]] =
    for
      arTrait <- refLoader.getTrait(expr.traitId)
      args <- ZIO.foreach(expr.arguments)(loadExprWithVariables)
    yield ArExpr(
      ExprConstructor.TraitType(arTrait),
      args,
    )

  private def loadExpr(expr: t.Expr)(using refLoader: ReferenceLoader, varContext: LocalVariableContext): Comp[WrapExpr] =
    def loadConstructor(ctor: t.Expr.Constructor): Comp[ExprConstructor] =
      ctor match {
        case t.Expr.Constructor.BindVariable(value) =>
          for
            local <- loadLocalVariableDefinition(value)
          yield ExprConstructor.BindVariable(local)

        case t.Expr.Constructor.ClassConstructorCall(value) =>
          for
            ctor <- refLoader.getClassConstructor(value.id)
          yield ExprConstructor.ClassConstructorCall(ctor)

        case t.Expr.Constructor.EnsureExecuted(_) =>
          ZIO.succeed(ExprConstructor.EnsureExecuted)

        case t.Expr.Constructor.FunctionCall(value) =>
          for
            ctor <- refLoader.getFunction(value.id)
          yield ExprConstructor.FunctionCall(ctor)

        case t.Expr.Constructor.FunctionObjectCall(_) =>
          ZIO.succeed(ExprConstructor.FunctionObjectCall)

        case t.Expr.Constructor.IfElse(ifElse) =>
          for
            whenTrue <- ZIO.foreach(ifElse.whenTrueProof)(loadLocalVariableDefinition)
            whenFalse <- ZIO.foreach(ifElse.whenFalseProof)(loadLocalVariableDefinition)
          yield ExprConstructor.IfElse(whenTrue, whenFalse)

        case t.Expr.Constructor.LoadConstantBool(value) =>
          ZIO.succeed(ExprConstructor.LoadConstantBool(value))

        case t.Expr.Constructor.LoadConstantInt(value) =>
          ZIO.succeed(ExprConstructor.LoadConstantInt(value))

        case t.Expr.Constructor.LoadConstantString(value) =>
          ZIO.succeed(ExprConstructor.LoadConstantString(value))

        case t.Expr.Constructor.LoadLambda(value) =>
          for
            local <- loadLocalVariableDefinition(value.argVariable)
          yield ExprConstructor.LoadLambda(local)

        case t.Expr.Constructor.LoadTuple(_) =>
          ZIO.succeed(ExprConstructor.LoadTuple)

        case t.Expr.Constructor.LoadTupleElement(value) =>
          ZIO.succeed(ExprConstructor.LoadTupleElement(value))

        case t.Expr.Constructor.LoadVariable(value) =>
          for
            local <- getVariable(value)
          yield ExprConstructor.LoadVariable(local)

        case t.Expr.Constructor.MethodCall(value) =>
          for
            ctor <- refLoader.getMethod(value.id)
          yield ExprConstructor.MethodCall(ctor)

        case t.Expr.Constructor.Proving(value) =>
          for
            witnesses <- ZIO.foreach(value.witnesses)(loadLocalVariableDefinition)
          yield ExprConstructor.Proving(witnesses)

        case t.Expr.Constructor.RaiseException(_) =>
          ZIO.succeed(ExprConstructor.RaiseException)

        case t.Expr.Constructor.Sequence(_) =>
          ZIO.succeed(ExprConstructor.Sequence)

        case t.Expr.Constructor.StoreVariable(value) =>
          for
            local <- getVariable(value)
          yield ExprConstructor.StoreVariable(local)

        case t.Expr.Constructor.TypeN(_) =>
          ZIO.succeed(ExprConstructor.TypeN)

        case t.Expr.Constructor.OmegaTypeN(value) =>
          ZIO.succeed(ExprConstructor.OmegaTypeN(value))

        case t.Expr.Constructor.AnyType(_) =>
          ZIO.succeed(ExprConstructor.AnyType)

        case t.Expr.Constructor.TraitType(value) =>
          for
            arTrait <- refLoader.getTrait(value.id)
          yield ExprConstructor.TraitType(arTrait)

        case t.Expr.Constructor.ClassType(value) =>
          for
            arClass <- refLoader.getClass(value.id)
          yield ExprConstructor.ClassType(arClass)

        case t.Expr.Constructor.FunctionType(_) =>
          ZIO.succeed(ExprConstructor.FunctionType)

        case t.Expr.Constructor.UnionType(_) =>
          ZIO.succeed(ExprConstructor.UnionType)

        case t.Expr.Constructor.IntersectionType(_) =>
          ZIO.succeed(ExprConstructor.IntersectionType)

        case t.Expr.Constructor.ExistentialType(value) =>
          for
            local <- loadLocalVariableDefinition(value.variable)
          yield ExprConstructor.ExistentialType(local)

        case t.Expr.Constructor.ConjunctionType(_) =>
          ZIO.succeed(ExprConstructor.ConjunctionType)

        case t.Expr.Constructor.DisjunctionType(_) =>
          ZIO.succeed(ExprConstructor.DisjunctionType)

        case t.Expr.Constructor.NeverType(_) =>
          ZIO.succeed(ExprConstructor.NeverType)

        case t.Expr.Constructor.SubtypeWitnessType(_) =>
          ZIO.succeed(ExprConstructor.SubtypeWitnessType)

        case t.Expr.Constructor.EqualToType(_) =>
          ZIO.succeed(ExprConstructor.EqualTo)

        case t.Expr.Constructor.AssumeErasedValue(_) =>
          ZIO.succeed(ExprConstructor.AssumeErasedValue)

        case _: t.Expr.Constructor.Empty.type =>
          ZIO.fail(InvalidTube("Unknown expression type"))
      }

    val loadArgsFirst = expr.constructor match {
      case t.Expr.Constructor.Proving(_) => true
      case _ => false
    }

    if loadArgsFirst then
      for
        args <- ZIO.foreach(expr.arguments)(loadExpr)
        ctor <- loadConstructor(expr.constructor)
        args <- ZIO.fromEither(
          ctor.argsFromExprs(args)
            .toRight {
              InvalidTube("Invalid arguments for expression")
            }
        )
      yield WrapExpr.OfExpr(ArExpr(ctor, args))
    else
      for
        ctor <- loadConstructor(expr.constructor)
        args <- ZIO.foreach(expr.arguments)(loadExpr)
        args <- ZIO.fromEither(
          ctor.argsFromExprs(args)
            .toRight { InvalidTube("Invalid arguments for expression") }
        )
      yield WrapExpr.OfExpr(ArExpr(ctor, args))

  end loadExpr



}
