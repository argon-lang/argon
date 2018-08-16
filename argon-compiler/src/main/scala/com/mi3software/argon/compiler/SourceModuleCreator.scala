package com.mi3software.argon.compiler

import com.mi3software.argon.parser
import com.mi3software.argon.parser.SourceAST
import com.mi3software.argon.util.{FileSpec, SourceLocation, WithSource}
import scalaz._
import Scalaz._
import scalaz.effect.IO

private[compiler] object SourceModuleCreator {


  def createModule[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (input: CompilerInput)
  : IO[TComp[ArModule[context.type]]] =
    loadReferenceModules[TComp](context)(input)
      .map { _.flatMap { refModules => createModuleWithRefs[TComp](context)(input)(refModules) } }

  private def createModuleWithRefs[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (input: CompilerInput)
  (referencedModules2: Vector[ArModuleWithPayload[context2.type, PayloadSpecifiers.ReferencePayloadSpecifier]])
    : TComp[ArModuleWithPayload[context2.type, PayloadSpecifiers.ReferencePayloadSpecifier]] =
    for {
      moduleElements <- input.source.traverseU { ast => createNamespaceElementFromAST[TComp](context2)(input.options)(referencedModules2)(ast) }

    } yield new ArModuleWithPayload[context2.type, PayloadSpecifiers.ReferencePayloadSpecifier] {
      override val context: context2.type = context2
      override val descriptor: ModuleDescriptor = input.options.moduleDescriptor
      override val globalNamespace: Namespace[ScopeValue[context.ContextScopeTypes]] =
        NamespaceBuilder.createNamespace(moduleElements)
      override val referencedModules: Vector[ArModule[context2.type]] = referencedModules2
    }


  private def loadReferenceModules[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (input: CompilerInput)
  : IO[TComp[Vector[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]]]] =
    ModuleLoader.loadReferencedModules(context)(input.references)

  private def createNamespaceElementFromAST[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (options: CompilerOptions)
  (referencedModules: Vector[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : TComp[ModuleElement[ScopeValue[context.ContextScopeTypes]]] =
    for {
      scope <- createScope[TComp](context)(referencedModules)(sourceAST)
      binding <- createNamespaceElementFromASTWithScope[TComp](context)(options)(scope)(sourceAST)
    } yield ModuleElement(sourceAST.currentNamespace, binding)

  private def createScope[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (referencedModules: Vector[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : TComp[Scope[context.ContextScopeTypes]] =
    Monad[TComp].point(EmptyScope())

  private def createNamespaceElementFromASTWithScope[TComp[+_] : Monad : Compilation]
  (context: ContextComp[TComp])
  (options: CompilerOptions)
  (scope: Scope[context.ContextScopeTypes])
  (sourceAST: SourceAST)
  : TComp[NamespaceBinding[ScopeValue[context.ContextScopeTypes]]] = {

    val expressionConverter = ??? : ExpressionConverterContext[context.type]

    def createBinding(name: String, modifiers: Vector[WithSource[parser.Modifier]])(f: (GlobalName, AccessModifierGlobal) => ScopeValue[context.ContextScopeTypes]): TComp[NamespaceBinding[ScopeValue[context.ContextScopeTypes]]] =
      parseGlobalAccessModifier[TComp](sourceAST.fileSpec, sourceAST.statement.location, getAccessModifiers(modifiers)).map { accessModifier =>
        val globalName = GlobalName.Normal(name)
        NamespaceBinding(
          globalName,
          accessModifier,
          f(globalName, accessModifier)
        )
      }

    sourceAST.statement.value match {
      case traitDeclarationStmt @ parser.TraitDeclarationStmt(_, Some(traitName), _, _, _, modifiers) =>
        createBinding(traitName, modifiers) { (globalName, accessModifier) =>
          val desc = TraitDescriptor.InNamespace(options.moduleDescriptor, sourceAST.currentNamespace, globalName, accessModifier)

          TraitScopeValue[context.ScopeTypesWithPayload[PayloadSpecifiers.DeclarationPayloadSpecifier]](
            SourceTrait[TComp](context)(scope)(traitDeclarationStmt)(desc)
          )
        }

      case funcDeclarationStmt @ parser.FunctionDeclarationStmt(Some(funcName), _, _, _, modifiers, _) =>
        createBinding(funcName, modifiers) { (globalName, accessModifier) =>
          val desc = FuncDescriptor.InNamespace(options.moduleDescriptor, sourceAST.currentNamespace, globalName, accessModifier)

          FunctionScopeValue[context.ScopeTypesWithPayload[PayloadSpecifiers.DeclarationPayloadSpecifier]](
            SourceFunction[TComp](context)(expressionConverter)(scope)(funcDeclarationStmt)(sourceAST.fileSpec)(desc)
          )
        }

      case _ => ???
    }
  }

  private def getAccessModifiers(modifiers: Vector[WithSource[parser.Modifier]]): List[WithSource[parser.AccessModifier]] =
    modifiers
      .collect {
        case WithSource(modifier: parser.AccessModifier, loc) => WithSource(modifier, loc)
      }
      .toList

  private def parseGlobalAccessModifier[TComp[+_] : Compilation](fileSpec: FileSpec, stmtLocation: SourceLocation, access: List[WithSource[parser.AccessModifier]]): TComp[AccessModifierGlobal] =
    parseAccessModifier[TComp](fileSpec, stmtLocation, access).flatMap {
      case accessModifier: AccessModifierGlobal => accessModifier.point[TComp]
      case AccessModifier.Private => AccessModifier.PrivateInternal.point[TComp]
      case accessModifier =>
        val loc = access.headOption.map(_.location).getOrElse(stmtLocation)
        Compilation[TComp].forErrors(CompilationError.AccessModifierNotAllowedForGlobal(accessModifier, CompilationMessageSource.SourceFile(fileSpec, loc)))
    }

  private def parseAccessModifier[TComp[+_] : Compilation](fileSpec: FileSpec, stmtLocation: SourceLocation, access: List[WithSource[parser.AccessModifier]]): TComp[AccessModifier] =
    access match {
      case WithSource(parser.PublicModifier, _) :: Nil =>
        AccessModifier.Public.point[TComp]

      case WithSource(a @ parser.PublicModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidAccessModifierCombination(a, b, CompilationMessageSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.ProtectedModifier, _) :: Nil =>
        AccessModifier.Protected.point[TComp]

      case WithSource(parser.ProtectedModifier, _) :: WithSource(parser.InternalModifier, _) :: Nil =>
        AccessModifier.ProtectedInternal.point[TComp]

      case WithSource(a @ parser.ProtectedModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidAccessModifierCombination(a, b, CompilationMessageSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.PrivateModifier, _) :: Nil =>
        AccessModifier.Protected.point[TComp]

      case WithSource(parser.PrivateModifier, _) :: WithSource(parser.InternalModifier, _) :: Nil =>
        AccessModifier.PrivateInternal.point[TComp]

      case WithSource(a @ parser.PrivateModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidAccessModifierCombination(a, b, CompilationMessageSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.InternalModifier, _) :: Nil =>
        AccessModifier.Protected.point[TComp]

      case WithSource(parser.InternalModifier, _) :: WithSource(parser.ProtectedModifier, _) :: Nil =>
        AccessModifier.ProtectedInternal.point[TComp]

      case WithSource(parser.InternalModifier, _) :: WithSource(parser.PrivateModifier, _) :: Nil =>
        AccessModifier.PrivateInternal.point[TComp]

      case WithSource(a @ parser.InternalModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidAccessModifierCombination(a, b, CompilationMessageSource.SourceFile(fileSpec, loc)))

      case Nil =>
        AccessModifier.Private.point[TComp]
    }


}
