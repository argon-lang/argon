package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.loaders.{ModuleLoader, NamespaceBuilder}
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.parser
import com.mi3software.argon.parser.SourceAST
import com.mi3software.argon.util.{FileSpec, NamespacePath, SourceLocation, WithSource}
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.PayloadSpecifiers._
import com.mi3software.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import scalaz.effect.IO

private[compiler] object SourceModuleCreator {


  def createModule[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (input: CompilerInput)
  : IO[TComp[ArModule[context.type, DeclarationPayloadSpecifier]]] =
    loadReferenceModules[TComp](context)(input)
      .map { _.flatMap { refModules => createModuleWithRefs[TComp](context)(input)(refModules) } }

  private def createModuleWithRefs[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (input: CompilerInput)
  (referencedModules2: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
    : TComp[ArModule[context2.type, DeclarationPayloadSpecifier]] =
    for {
      moduleElements <- input.source.traverseU { ast => createNamespaceElementFromAST[TComp](context2)(input.options)(referencedModules2)(ast) }

    } yield new ArModule[context2.type, DeclarationPayloadSpecifier] {
      override val context: context2.type = context2
      override val descriptor: ModuleDescriptor = input.options.moduleDescriptor
      override val globalNamespace: Namespace[context.type, DeclarationPayloadSpecifier] =
        NamespaceBuilder.createNamespace[context.type, DeclarationPayloadSpecifier](moduleElements)
      override val referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]] = referencedModules2
    }


  private def loadReferenceModules[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (input: CompilerInput)
  : IO[TComp[Vector[ArModule[context.type, ReferencePayloadSpecifier]]]] =
    ModuleLoader.loadReferencedModules(context)(input.references)

  private def createNamespaceElementFromAST[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (options: CompilerOptions)
  (referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : TComp[ModuleElement[context2.type, DeclarationPayloadSpecifier]] =
    for {
      scope <- createScope[TComp](context2)(referencedModules)(sourceAST)
      envF = (envFileSpec: FileSpec) => new EnvCreator[context2.type] {
        override def apply(context: context2.type)(descriptor: Descriptor): ExpressionConverter.Env[context.type, context.scopeContext.Scope] =
          ExpressionConverter.Env(
            descriptor = descriptor,
            fileSpec = fileSpec,
            referencedModules = referencedModules,
            scope = scope,
          )

        override val fileSpec: FileSpec = envFileSpec
      }
      binding <- createNamespaceElementFromASTWithScope[TComp](context2)(options)(envF)(sourceAST)
    } yield ModuleElement(sourceAST.currentNamespace, binding)

  private def createScope[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : TComp[context.scopeContext.Scope] =
    Monad[TComp].point(context.scopeContext.EmptyScope())

  private def createNamespaceElementFromASTWithScope[TComp[+_] : Monad : Compilation]
  (context: ContextComp[TComp])
  (options: CompilerOptions)
  (envF: FileSpec => EnvCreator[context.type])
  (sourceAST: SourceAST)
  : TComp[GlobalBinding[context.type, DeclarationPayloadSpecifier]] = {

    val env = envF(sourceAST.fileSpec)

    def createBinding(name: String, modifiers: Vector[WithSource[parser.Modifier]])(f: (GlobalName, AccessModifierGlobal) => GlobalBinding[context.type, DeclarationPayloadSpecifier]): TComp[GlobalBinding[context.type, DeclarationPayloadSpecifier]] =
      parseGlobalAccessModifier[TComp](sourceAST.fileSpec, sourceAST.statement.location, getAccessModifiers(modifiers)).map { accessModifier =>
        val globalName = GlobalName.Normal(name)
        f(globalName, accessModifier)
      }

    sourceAST.statement.value match {
      case traitDeclarationStmt @ parser.TraitDeclarationStmt(_, Some(traitName), _, _, _, modifiers) =>
        createBinding(traitName, modifiers) { (globalName, accessModifier) =>
          val desc = TraitDescriptor.InNamespace(options.moduleDescriptor, sourceAST.currentNamespace, globalName, accessModifier)

          GlobalBinding.GlobalTrait(
            globalName, accessModifier,
            SourceTrait[TComp](context)(env)(traitDeclarationStmt)(desc)
          )
        }

      case funcDeclarationStmt @ parser.FunctionDeclarationStmt(Some(funcName), _, _, _, modifiers, _) =>
        createBinding(funcName, modifiers) { (globalName, accessModifier) =>
          val desc = FuncDescriptor.InNamespace(options.moduleDescriptor, sourceAST.currentNamespace, globalName, accessModifier)

          GlobalBinding.GlobalFunction(
            globalName, accessModifier,
            SourceFunction[TComp](context)(env)(funcDeclarationStmt)(desc)
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
