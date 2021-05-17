package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.parser
import dev.argon.util._
import cats.evidence.Is
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import shapeless.{Id, Nat}
import zio.IO

object SourceMethod {
  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (stmt: parser.MethodDeclarationStmt, location: SourceLocation)
  (methodOwner: MethodOwner[context2.type, DeclarationPayloadSpecifier])
  : Comp[ArMethod[context2.type, DeclarationPayloadSpecifier]] = {
    
    for {
      uniqId <- UniqueIdentifier.make
      sigCache <- ValueCache.make[CompilationError, context2.signatureContext.Signature[FunctionResultInfo, _ <: Nat]]
      implCache <- ValueCache.make[CompilationError, context2.TMethodImplementation]
    } yield new ArMethod[context2.type, DeclarationPayloadSpecifier] {
      override val context: context2.type = context2
      override val contextProof: context.type Is context2.type = Is.refl

      import context.scopeContext.ScopeExtensions


      override val id: MethodId = MethodId(uniqId)
      override val name: MethodName = MethodName.fromMethodNameSpecifier(stmt.name)
      override val fileId: FileID = env.fileSpec.fileID


      override val isAbstract: Boolean = stmt.modifiers.exists {
        case WithSource(parser.AbstractModifier, _) => true
        case _ => false
      }

      override val isImplicitOverride: Boolean = stmt.modifiers.exists {
        case WithSource(parser.OverrideModifier, _) => true
        case _ => false
      }

      override val isFinal: Boolean = stmt.modifiers.exists {
        case WithSource(parser.FinalModifier, _) => true
        case _ => false
      }

      override val isVirtual: Boolean = isAbstract || isImplicitOverride || isFinal || stmt.modifiers.exists {
        case WithSource(parser.VirtualModifier, _) => true
        case _ => false
      }

      override val effectInfo: EffectInfo = EffectInfo(stmt.purity)


      override val owner: MethodOwner[context.type, DeclarationPayloadSpecifier] = methodOwner

      private def localVarOwner = LocalVariableOwner.ByMethod(AbsRef(this))
      private def paramVarOwner = ParameterVariableOwner.ByMethod(AbsRef(this))

      override lazy val signatureUnsubstituted: Comp[context.signatureContext.Signature[FunctionResultInfo, _ <: Nat]] =
        sigCache.get(
          SourceSignatureCreator.fromParameters[FunctionResultInfo](context2)(
            env(context)(effectInfo, id, localVarOwner)
          )(paramVarOwner)(stmt.parameters)(resultCreator(stmt.returnType))
        )



      override lazy val payload: Comp[context.TMethodImplementation] =
        implCache.get(
          if(isAbstract)
            IO.succeed(context.abstractMethodImplementation)
          else
            stmt.body match {
              case Some(WithSource(parser.ExternExpr(specifier), location)) =>
                context.createExternMethodImplementation(specifier, DiagnosticSource.SourceFile(env.fileSpec, location))

              case Some(body) =>
                for {
                  sig <- signatureUnsubstituted
                  env2 = env(context)(effectInfo, id, localVarOwner)
                  env3 = env2.copy(scope = env2.scope.addParameters(
                    sig.unsubstitutedParameters
                  ))
                  expr <- ExpressionConverter.convertExpression(context)(env3)(sig.unsubstitutedResult.returnType)(body)
                } yield context.createExprMethodImplementation(expr)

              case None =>
                Compilation.forErrors(DiagnosticError.NonAbstractMethodNotImplemented(AbsRef(this), DiagnosticSource.SourceFile(env.fileSpec, location)))
            }
        )


      private def resultCreator(returnTypeExpr: WithSource[parser.Expr]): ResultCreator.Aux[context2.type, FunctionResultInfo] =  new ResultCreator[FunctionResultInfo] {

        override val context: context2.type = context2

        override def createResult
        (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
        : Comp[FunctionResultInfo[context.type, context.typeSystem.TTypeWrapper]] = {
          ExpressionConverter.convertTypeExpression(context)(env)(returnTypeExpr)
            .map { t => FunctionResultInfo[context.type, Id](t) }
        }
      }

    }
  }

}
