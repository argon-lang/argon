package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.{ModuleLoad, ModuleLoader, NamespaceBuilder, ResourceIndicator, ResourceReader, SourceParser}
import dev.argon.compiler.lookup._
import dev.argon.parser
import dev.argon.parser.SourceAST
import dev.argon.util._
import cats.{Id => _, _}
import cats.implicits._
import PayloadSpecifiers._
import dev.argon.compiler.expr.{Parameter, Variable}
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.options.{CompilerInput, CompilerOptions}
import shapeless.Id
import zio._
import zio.stream._
import zio.interop.catz.core._

private[compiler] object SourceModuleCreator extends AccessModifierHelpers {


  def createModule[I <: ResourceIndicator: Tag, TContext <: Context.WithRes[I]: Tag]
  (context: TContext)
  (input: CompilerInput[I, context.BackendOptions])
  : ZManaged[ModuleLoad[I, TContext] with ResourceReader[I] with SourceParser, ErrorList, ArModule[context.type, DeclarationPayloadSpecifier]] =
    ModuleLoader.loadReferencedModules[I, TContext](context)(input.options.references.files.toVector).mapM { refModules =>
      createModuleWithRefs(context)(input)(refModules)
    }

  private def createModuleWithRefs[I <: ResourceIndicator: Tag]
  (context2: Context)
  (input: CompilerInput[I, context2.BackendOptions])
  (referencedModules2: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
    : RComp[ResourceReader[I] with SourceParser, ArModule[context2.type, DeclarationPayloadSpecifier]] = {
    for {
      env <- ZIO.environment[ResourceReader[I] with SourceParser]
      globalNamespaceCache <- ValueCache.make[ErrorList, Namespace[context2.type, DeclarationPayloadSpecifier]]
    } yield new ArModule[context2.type, DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      override val descriptor: ModuleId = ModuleId(input.options.moduleName)
      override val globalNamespace: Comp[Namespace[context.type, DeclarationPayloadSpecifier]] =
        globalNamespaceCache.get(
          NamespaceBuilder.createNamespace[ResourceReader[I] with SourceParser, context.type, DeclarationPayloadSpecifier](
            Stream.fromIterable(input.options.inputFiles.files)
              .zipWithIndex
              .flatMap { case (file, index) =>
                val fileSpec = FileSpec(FileID(index.toInt), file.show)

                Stream.unwrap(
                  ZIO.access[ResourceReader[I]](_.get.readTextFile(file))
                    .flatMap { fileStream =>
                      ZIO.access[SourceParser](_.get.parse(fileSpec)(fileStream))
                    }
                )
              }
              .mapM { ast =>
                createNamespaceElementFromAST(context2)(input.options)(this)(referencedModules2)(ast)
              }
          ).provide(env)
        )

      override val referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]] = referencedModules2
    }
  }

  private def createNamespaceElementFromAST
  (context2: Context)
  (options: CompilerOptions[Id, _])
  (currentModule: ArModule[context2.type, DeclarationPayloadSpecifier])
  (referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : Comp[ModuleElement[context2.type, DeclarationPayloadSpecifier]] =
    createScope(context2)(currentModule)(referencedModules)(sourceAST).flatMap { scope =>

      import context2.scopeContext.ScopeExtensions

      val currentModule2 = currentModule
      val referencedModules2 = referencedModules

      final class EnvCreatorInstance(envFileSpec: FileSpec, scope: context2.scopeContext.Scope) extends EnvCreator[context2.type] {
        override def apply(context: context2.type)(effectInfo: EffectInfo, callerId: CallerId, varOwner: LocalVariableOwner[context.type]): ExpressionConverter.Env[context.type, context.scopeContext.Scope] =
          ExpressionConverter.Env(
            effectInfo = effectInfo,
            callerId = callerId,
            variableOwner = varOwner,
            fileSpec = fileSpec,
            currentModule = currentModule,
            referencedModules = referencedModules,
            scope = scope,
            allowAbstractConstructor = false,
          )


        override def addVariables(context: context2.type)(variables: Vector[Variable[context.type, Id]]): EnvCreator[context2.type] =
          new EnvCreatorInstance(envFileSpec, scope.addVariables(variables))

        override def addParameters(context: context2.type)(params: Vector[Parameter[context.type, Id]]): EnvCreator[context2.type] =
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
  (options: CompilerOptions[Id, _])
  (envF: FileSpec => EnvCreator[context.type])
  (sourceAST: SourceAST)
  : Comp[GlobalBinding[context.type, DeclarationPayloadSpecifier]] = {

    import context._

    val env = envF(sourceAST.fileSpec)
    val moduleId = ModuleId(options.moduleName)

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
          val owner = TraitOwner.ByNamespace(moduleId, sourceAST.currentNamespace, globalName)

          for {
            arTrait <- SourceTrait(context)(env)(traitDeclarationStmt)(owner)
          } yield GlobalBinding.GlobalTrait(globalName, accessModifier, arTrait)
        }

      case classDeclarationStmt @ parser.ClassDeclarationStmt(_, WithSource(className, _), _, _, _, modifiers) =>
        createBinding(className, modifiers) { (globalName, accessModifier) =>
          val owner = ClassOwner.ByNamespace(moduleId, sourceAST.currentNamespace, globalName)

          for {
            arClass <- SourceClass(context)(env)(classDeclarationStmt)(owner)
          } yield GlobalBinding.GlobalClass(globalName, accessModifier, arClass)
        }

      case funcDeclarationStmt @ parser.FunctionDeclarationStmt(funcName, _, _, _, modifiers, _) =>
        createBinding(funcName, modifiers) { (globalName, accessModifier) =>
          val owner = FunctionOwner.ByNamespace(moduleId, sourceAST.currentNamespace, globalName)

          for {
            arFunc <- SourceFunction(context)(env)(funcDeclarationStmt)(owner)
          } yield GlobalBinding.GlobalFunction(
            globalName, accessModifier,
            arFunc
          )
        }

      case dataCtorDeclarationStmt @ parser.DataConstructorDeclarationStmt(WithSource(name, _), _, _, _, modifiers) =>
        createBinding(name, modifiers) { (globalName, accessModifier) =>
          val owner = DataConstructorOwner.ByNamespace(moduleId, sourceAST.currentNamespace, globalName)

          for {
            ctor <- SourceDataConstructor(context)(env)(dataCtorDeclarationStmt)(owner)
          } yield GlobalBinding.GlobalDataConstructor(globalName, accessModifier, ctor)
        }

      case _ => ???
    }
  }


}
