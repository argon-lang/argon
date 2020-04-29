package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.{ModuleLoad, ModuleLoader, NamespaceBuilder, ResourceIndicator}
import dev.argon.compiler.lookup._
import dev.argon.parser
import dev.argon.parser.SourceAST
import dev.argon.util._
import cats._
import cats.implicits._
import PayloadSpecifiers._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import zio._
import zio.interop.catz._

private[compiler] object SourceModuleCreator extends AccessModifierHelpers {


  def createModule[I <: ResourceIndicator: Tagged, TContext <: Context.WithRes[I]: Tagged]
  (context: TContext)
  (input: CompilerInput[I, context.BackendOptions])
  : ZManaged[ModuleLoad[I, TContext], ErrorList, ArModule[context.type, DeclarationPayloadSpecifier]] =
    ModuleLoader.loadReferencedModules[I, TContext](context)(input.references).mapM { refModules =>
      createModuleWithRefs(context)(input)(refModules)
    }

  private def createModuleWithRefs
  (context2: Context)
  (input: CompilerInput[context2.ResIndicator, context2.BackendOptions])
  (referencedModules2: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
    : Comp[ArModule[context2.type, DeclarationPayloadSpecifier]] = {
    for {
      globalNamespaceCache <- ValueCache.make[ErrorList, Namespace[context2.type, DeclarationPayloadSpecifier]]
    } yield new ArModule[context2.type, DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      override val descriptor: ModuleDescriptor = ModuleDescriptor(input.options.moduleName)
      override val globalNamespace: Comp[Namespace[context.type, DeclarationPayloadSpecifier]] =
        globalNamespaceCache.get(
          input.source
            .traverse { ast =>
              createNamespaceElementFromAST(context2)(input.options)(this)(referencedModules2)(ast)
            }
            .map(NamespaceBuilder.createNamespace[context.type, DeclarationPayloadSpecifier])
        )

      override val referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]] = referencedModules2
    }
  }

  private def createNamespaceElementFromAST
  (context2: Context)
  (options: CompilerOptions[Id])
  (currentModule: ArModule[context2.type, DeclarationPayloadSpecifier])
  (referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : Comp[ModuleElement[context2.type, DeclarationPayloadSpecifier]] =
    createScope(context2)(currentModule)(referencedModules)(sourceAST).flatMap { scope =>

      import context2.scopeContext.ScopeExtensions

      val currentModule2 = currentModule
      val referencedModules2 = referencedModules

      final class EnvCreatorInstance(envFileSpec: FileSpec, scope: context2.scopeContext.Scope) extends EnvCreator[context2.type] {
        override def apply(context: context2.type)(effectInfo: EffectInfo, descriptor: VariableOwnerDescriptor): ExpressionConverter.Env[context.type, context.scopeContext.Scope] =
          ExpressionConverter.Env(
            effectInfo = effectInfo,
            descriptor = descriptor,
            fileSpec = fileSpec,
            currentModule = currentModule,
            referencedModules = referencedModules,
            scope = scope,
            allowAbstractConstructor = false,
          )


        override def addVariables(context: context2.type)(variables: Vector[context.typeSystem.Variable]): EnvCreator[context2.type] =
          new EnvCreatorInstance(envFileSpec, scope.addVariables(variables))

        override def addParameters(context: context2.type)(params: Vector[context.typeSystem.Parameter]): EnvCreator[context2.type] =
          new EnvCreatorInstance(envFileSpec, scope.addParameters(params))

        override val fileSpec: FileSpec = envFileSpec
        override val currentModule: ArModule[context2.type, DeclarationPayloadSpecifier] = currentModule2
        override val referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]] = referencedModules2
      }

      val envF = (envFileSpec: FileSpec) => new EnvCreatorInstance(envFileSpec, scope)
      createNamespaceElementFromASTWithScope(context2)(options)(envF)(sourceAST).map { binding =>
        ModuleElement(sourceAST.currentNamespace, binding)
      }
    }

  private def createScope
  (context: Context)
  (currentModule: ArModule[context.type, DeclarationPayloadSpecifier])
  (referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : Comp[context.scopeContext.Scope] =
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

  private def createId(sourceAST: SourceAST): BigInt = {
    val a: BigInt = sourceAST.fileSpec.fileID.id
    val b: BigInt = sourceAST.index

    (a + b) * (a + b + 1) / 2 + b
  }

  private def createNamespaceElementFromASTWithScope
  (context: Context)
  (options: CompilerOptions[Id])
  (envF: FileSpec => EnvCreator[context.type])
  (sourceAST: SourceAST)
  : Comp[GlobalBinding[context.type, DeclarationPayloadSpecifier]] = {

    import context._

    val env = envF(sourceAST.fileSpec)
    val moduleDescriptor = ModuleDescriptor(options.moduleName)

    def createBinding(name: Option[String], modifiers: Vector[WithSource[parser.Modifier]])(f: (GlobalName, AccessModifierGlobal) => Comp[GlobalBinding[context.type, DeclarationPayloadSpecifier]]): Comp[GlobalBinding[context.type, DeclarationPayloadSpecifier]] =
      parseGlobalAccessModifier(sourceAST.fileSpec, sourceAST.statement.location, getAccessModifiers(modifiers)).flatMap { accessModifier =>
        val globalName = name match {
          case Some(n) => GlobalName.Normal(n)
          case None => GlobalName.Unnamed
        }

        f(globalName, accessModifier)
      }

    sourceAST.statement.value match {
      case traitDeclarationStmt @ parser.TraitDeclarationStmt(_, traitName, _, _, _, modifiers) =>
        createBinding(traitName, modifiers) { (globalName, accessModifier) =>
          val desc = TraitDescriptor.InNamespace(moduleDescriptor, createId(sourceAST), sourceAST.currentNamespace, globalName)

          for {
            arTrait <- SourceTrait(context)(env)(traitDeclarationStmt)(desc)
          } yield GlobalBinding.GlobalTrait(globalName, accessModifier, arTrait)
        }

      case classDeclarationStmt @ parser.ClassDeclarationStmt(_, WithSource(className, _), _, _, _, modifiers) =>
        createBinding(className, modifiers) { (globalName, accessModifier) =>
          val desc = ClassDescriptor.InNamespace(moduleDescriptor, createId(sourceAST), sourceAST.currentNamespace, globalName)

          for {
            arClass <- SourceClass(context)(env)(classDeclarationStmt)(desc)
          } yield GlobalBinding.GlobalClass(globalName, accessModifier, arClass)
        }

      case funcDeclarationStmt @ parser.FunctionDeclarationStmt(funcName, _, _, _, modifiers, _) =>
        createBinding(funcName, modifiers) { (globalName, accessModifier) =>
          val desc = FuncDescriptor.InNamespace(moduleDescriptor, createId(sourceAST), sourceAST.currentNamespace, globalName)

          GlobalBinding.GlobalFunction(
            globalName, accessModifier,
            SourceFunction(context)(env)(funcDeclarationStmt)(desc)
          ).pure[Comp]
        }

      case dataCtorDeclarationStmt @ parser.DataConstructorDeclarationStmt(WithSource(name, _), _, _, _, modifiers) =>
        createBinding(name, modifiers) { (globalName, accessModifier) =>
          val desc = DataConstructorDescriptor.InNamespace(moduleDescriptor, createId(sourceAST), sourceAST.currentNamespace, globalName)


          for {
            ctor <- SourceDataConstructor(context)(env)(dataCtorDeclarationStmt)(desc)
          } yield GlobalBinding.GlobalDataConstructor(globalName, accessModifier, ctor)
        }

      case _ => ???
    }
  }


}
