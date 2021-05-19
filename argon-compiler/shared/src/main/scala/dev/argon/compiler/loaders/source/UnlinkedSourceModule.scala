package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.UnlinkedModule
import dev.argon.compiler.lookup._
import dev.argon.parser
import dev.argon.parser.{NameSpecifier, SourceAST}
import dev.argon.util._
import cats.implicits._
import PayloadSpecifiers._
import dev.argon.compiler.expr.{Parameter, Variable}
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import zio._
import zio.stream._
import AccessModifierHelpers._

final class UnlinkedSourceModule[TContext <: Context]
(
  override val descriptor: ModuleId,
  override val referencedModules: Vector[ModuleId],
  sourceCodeStream: CompStream[SourceAST]
) extends UnlinkedModule[TContext, DeclarationPayloadSpecifier] {


  override def load(context2: TContext)(referencedModules2: Vector[ArModule[context2.type, ReferencePayloadSpecifier]]): CompManaged[ArModule[context2.type, DeclarationPayloadSpecifier]] =
    for {
      parsedCodeStream <- StreamMemo.make(sourceCodeStream)

      namespacesStream <- StreamMemo.make(
        distinctStream(parsedCodeStream.map { _.currentNamespace })
      )

      bindingStreamCache <- ValueCacheManaged.make[CompilationError, Stream[CompilationError, ModuleElement[context2.type, DeclarationPayloadSpecifier]]]
    } yield new ArModule[context2.type, DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      override val id: ModuleId = UnlinkedSourceModule.this.descriptor
      override val namespaces: CompStream[NamespacePath] = namespacesStream
      override val referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]] = referencedModules2

      override def getNamespace(ns: NamespacePath): CompStream[GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] =
        bindingStream
          .filter { element => element.namespacePath === ns }
          .map { element => element.binding }

      override def bindings: CompStream[GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] =
        bindingStream.map { element => element.binding }

      private def bindingStream: Stream[CompilationError, ModuleElement[context2.type, DeclarationPayloadSpecifier]] =
        ZStream.unwrap(
          bindingStreamCache.get(StreamMemo.make(
            parsedCodeStream.mapM { sourceAST =>
              createNamespaceElementFromAST(context2)(this)(referencedModules2)(sourceAST)
            }
          ))
        )

    }


  private def createNamespaceElementFromAST
  (context2: Context)
  (currentModule: ArModule[context2.type, DeclarationPayloadSpecifier])
  (referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : Comp[ModuleElement[context2.type, DeclarationPayloadSpecifier]] =
    createScope(context2)(currentModule)(referencedModules)(sourceAST).flatMap { scope =>

      import context2.scopeContext.ScopeExtensions

      val currentModule2 = currentModule
      val referencedModules2 = referencedModules

      final class EnvCreatorInstance(envFileSpec: FileSpec, scope: context2.scopeContext.Scope, accessTokens: Set[AccessToken]) extends EnvCreator[context2.type] {
        override def apply(context: context2.type)(effectInfo: EffectInfo, callerId: CallerId, varOwner: LocalVariableOwner[context2.type]): ExpressionConverter.Env[context.type, context.scopeContext.Scope] =
          ExpressionConverter.Env(
            effectInfo = effectInfo,
            callerId = callerId,
            variableOwner = varOwner,
            fileSpec = fileSpec,
            currentModule = currentModule,
            referencedModules = referencedModules,
            scope = scope,
            allowAbstractConstructor = false,
            accessTokens = accessTokens,
          )


        override def addVariables(context: context2.type)(variables: Vector[Variable[context.type, Id]]): EnvCreator[context2.type] =
          new EnvCreatorInstance(envFileSpec, scope.addVariables(variables), accessTokens)

        override def addParameters(context: context2.type)(params: Vector[Parameter[context.type, Id]]): EnvCreator[context2.type] =
          new EnvCreatorInstance(envFileSpec, scope.addParameters(params), accessTokens)

        override def addAccessToken(accessToken: AccessToken): EnvCreator[context2.type] =
          new EnvCreatorInstance(envFileSpec, scope, accessTokens + accessToken)

        override val fileSpec: FileSpec = envFileSpec
        override val currentModule: ArModule[context2.type, DeclarationPayloadSpecifier] = currentModule2
        override val referencedModules: Vector[ArModule[context2.type, ReferencePayloadSpecifier]] = referencedModules2
      }

      val envF = (envFileSpec: FileSpec) => new EnvCreatorInstance(envFileSpec, scope, Set.empty)
      createNamespaceElementFromASTWithScope(context2)(currentModule)(envF)(sourceAST).map { binding =>
        ModuleElement(sourceAST.currentNamespace, binding)
      }
    }

  private def distinctStream[R, E, A](stream: ZStream[R, E, A]): ZStream[R, E, A] =
    ZStream.unwrap(
      for {
        seenElements <- Ref.make(Set.empty[A])
      } yield stream.filterM { a =>
        seenElements.get.flatMap { seen =>
          if(seen.contains(a))
            IO.succeed(false)
          else
            seenElements.update { _ + a }.as(true)
        }
      }
    )

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
        referencedModules.map(AbsRef(_)),
      )
    )

  

  private def createNamespaceElementFromASTWithScope
  (context: Context)
  (currentModule: ArModule[context.type, DeclarationPayloadSpecifier])
  (envF: FileSpec => EnvCreator[context.type])
  (sourceAST: SourceAST)
  : Comp[GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] = {


    val env = envF(sourceAST.fileSpec)

    def createBinding(name: parser.NameSpecifier, modifiers: Vector[WithSource[parser.Modifier]])(f: (GlobalName, AccessModifierGlobal) => Comp[GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]]): Comp[GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] =
      parseGlobalAccessModifier(sourceAST.fileSpec, sourceAST.statement.location, getAccessModifiers(modifiers)).flatMap { accessModifier =>
        val globalName = name match {
          case NameSpecifier.Identifier(n) => GlobalName.Normal(n)
          case NameSpecifier.Operator(op: parser.Token.BinaryOperatorToken) => GlobalName.Operator(op.binaryOperator.symbol)
          case NameSpecifier.Operator(op: parser.Token.UnaryOperatorToken) => GlobalName.Operator(op.unaryOperator.symbol)
          case NameSpecifier.Blank => GlobalName.Unnamed
        }

        f(globalName, accessModifier)
      }

    sourceAST.statement.value match {
      case traitDeclarationStmt @ parser.TraitDeclarationStmt(_, traitName, _, _, _, modifiers) =>
        createBinding(traitName, modifiers) { (globalName, accessModifier) =>
          val owner = TraitOwner.ByNamespace(currentModule, sourceAST.currentNamespace, globalName)

          for {
            arTrait <- SourceTrait(context)(env)(traitDeclarationStmt)(owner).memoize
          } yield GlobalBinding.GlobalTrait(globalName, accessModifier, None, arTrait)
        }

      case classDeclarationStmt @ parser.ClassDeclarationStmt(_, WithSource(className, _), _, _, _, modifiers) =>
        createBinding(className, modifiers) { (globalName, accessModifier) =>
          val owner = ClassOwner.ByNamespace(currentModule, sourceAST.currentNamespace, globalName)

          for {
            arClass <- SourceClass(context)(env)(classDeclarationStmt)(owner).memoize
          } yield GlobalBinding.GlobalClass(globalName, accessModifier, None, arClass)
        }

      case funcDeclarationStmt @ parser.FunctionDeclarationStmt(funcName, _, _, _, modifiers, _) =>
        createBinding(funcName, modifiers) { (globalName, accessModifier) =>
          val owner = FunctionOwner.ByNamespace(currentModule, sourceAST.currentNamespace, globalName)

          for {
            arFunc <- SourceFunction(context)(env)(funcDeclarationStmt)(owner).memoize
          } yield GlobalBinding.GlobalFunction(
            globalName, accessModifier, None,
            arFunc
          )
        }

      case dataCtorDeclarationStmt @ parser.DataConstructorDeclarationStmt(WithSource(name, _), _, _, _, modifiers) =>
        createBinding(name, modifiers) { (globalName, accessModifier) =>
          val owner = DataConstructorOwner.ByNamespace(currentModule, sourceAST.currentNamespace, globalName)

          for {
            ctor <- SourceDataConstructor(context)(env)(dataCtorDeclarationStmt)(owner).memoize
          } yield GlobalBinding.GlobalDataConstructor(globalName, accessModifier, None, ctor)
        }

      case _ => ???
    }
  }


}
