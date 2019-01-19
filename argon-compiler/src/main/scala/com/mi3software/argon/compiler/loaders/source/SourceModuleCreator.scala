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
import PayloadSpecifiers._
import com.mi3software.argon.compiler.loaders.source.ExpressionConverter.EnvCreator

private[compiler] object SourceModuleCreator extends AccessModifierHelpers {


  def createModule[TComp[+_] : Compilation, I: Show]
  (context: ContextComp[TComp])
  (input: CompilerInput[I])
  (implicit res: ResourceAccess[TComp, I])
  : TComp[ArModule[context.type, DeclarationPayloadSpecifier]] =
    loadReferenceModules[TComp, I](context)(input)
      .flatMap { refModules => createModuleWithRefs[TComp, I](context)(input)(refModules) }

  private def createModuleWithRefs[TComp[+_] : Compilation, I]
  (context2: ContextComp[TComp])
  (input: CompilerInput[I])
  (referencedModules2: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
    : TComp[ArModule[context2.type, DeclarationPayloadSpecifier]] = for {
    globalNamespaceCache <- Compilation[TComp].createCache[Namespace[context2.type, DeclarationPayloadSpecifier]]
  } yield new ArModule[context2.type, DeclarationPayloadSpecifier] {
      override val context: context2.type = context2
      override val descriptor: ModuleDescriptor = input.options.moduleDescriptor
      override val globalNamespace: TComp[Namespace[context.type, DeclarationPayloadSpecifier]] =
        globalNamespaceCache(
          input.source
            .traverse { ast =>
              createNamespaceElementFromAST[TComp](context2)(input.options)(this)(referencedModules2)(ast)
            }
            .map(NamespaceBuilder.createNamespace[context.type, DeclarationPayloadSpecifier])
        )

      override val referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]] = referencedModules2
    }


  private def loadReferenceModules[TComp[+_] : Compilation, I: Show]
  (context: ContextComp[TComp])
  (input: CompilerInput[I])
  (implicit res: ResourceAccess[TComp, I])
  : TComp[Vector[ArModule[context.type, ReferencePayloadSpecifier]]] =
    ModuleLoader.loadReferencedModules(context)(input.references)

  private def createNamespaceElementFromAST[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (options: CompilerOptions)
  (currentModule: ArModule[context2.type, DeclarationPayloadSpecifier])
  (referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : TComp[ModuleElement[context2.type, DeclarationPayloadSpecifier]] =
    createScope[TComp](context2)(currentModule)(referencedModules)(sourceAST).flatMap { scope =>

      import context2.scopeContext.ScopeExtensions

      final class EnvCreatorInstance(envFileSpec: FileSpec, scope: context2.scopeContext.Scope) extends EnvCreator[context2.type] {
        override def apply(context: context2.type)(effectInfo: EffectInfo, descriptor: VariableOwnerDescriptor): ExpressionConverter.Env[context.type, context.scopeContext.Scope] =
          ExpressionConverter.Env(
            effectInfo = effectInfo,
            descriptor = descriptor,
            fileSpec = fileSpec,
            currentModule = currentModule,
            referencedModules = referencedModules,
            scope = scope,
          )


        override def addVariables(context: context2.type)(variables: Vector[context.typeSystem.Variable[VariableLikeDescriptor]]): EnvCreator[context2.type] =
          new EnvCreatorInstance(envFileSpec, scope.addVariables(variables))

        override val fileSpec: FileSpec = envFileSpec
      }

      val envF = (envFileSpec: FileSpec) => new EnvCreatorInstance(envFileSpec, scope)
      createNamespaceElementFromASTWithScope[TComp](context2)(options)(envF)(sourceAST).map { binding =>
        ModuleElement(sourceAST.currentNamespace, binding)
      }
    }

  private def createScope[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (currentModule: ArModule[context.type, DeclarationPayloadSpecifier])
  (referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : TComp[context.scopeContext.Scope] =
    GlobalScope.createNSScope(context)(
      Vector(
        Vector(sourceAST.currentNamespace),
        sourceAST.importNamespaces,
      )
    )(
      Vector(
        Vector(AbsRef(currentModule)),
        referencedModules.map(AbsRef.apply),
      )
    )(
      context.scopeContext.EmptyScope
    )

  private def createNamespaceElementFromASTWithScope[TComp[+_] : Monad : Compilation]
  (context: ContextComp[TComp])
  (options: CompilerOptions)
  (envF: FileSpec => EnvCreator[context.type])
  (sourceAST: SourceAST)
  : TComp[GlobalBinding[context.type, DeclarationPayloadSpecifier]] = {

    val env = envF(sourceAST.fileSpec)

    def createBinding(name: String, modifiers: Vector[WithSource[parser.Modifier]])(f: (GlobalName, AccessModifierGlobal) => TComp[GlobalBinding[context.type, DeclarationPayloadSpecifier]]): TComp[GlobalBinding[context.type, DeclarationPayloadSpecifier]] =
      parseGlobalAccessModifier[TComp](sourceAST.fileSpec, sourceAST.statement.location, getAccessModifiers(modifiers)).flatMap { accessModifier =>
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
          ).point[TComp]
        }

      case classDeclarationStmt @ parser.ClassDeclarationStmt(_, Some(className), _, _, _, modifiers) =>
        createBinding(className, modifiers) { (globalName, accessModifier) =>
          val desc = ClassDescriptor.InNamespace(options.moduleDescriptor, sourceAST.currentNamespace, globalName, accessModifier)

          for {
            arClass <- SourceClass[TComp](context)(env)(classDeclarationStmt)(desc)
          } yield GlobalBinding.GlobalClass(globalName, accessModifier, arClass)
        }

      case funcDeclarationStmt @ parser.FunctionDeclarationStmt(Some(funcName), _, _, _, modifiers, _) =>
        createBinding(funcName, modifiers) { (globalName, accessModifier) =>
          val desc = FuncDescriptor.InNamespace(options.moduleDescriptor, sourceAST.currentNamespace, globalName, accessModifier)

          GlobalBinding.GlobalFunction(
            globalName, accessModifier,
            SourceFunction[TComp](context)(env)(funcDeclarationStmt)(desc)
          ).point[TComp]
        }

      case _ => ???
    }
  }


}
