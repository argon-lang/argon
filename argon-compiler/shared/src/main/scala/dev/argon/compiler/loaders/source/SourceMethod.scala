package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.parser
import dev.argon.util._
import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import shapeless.Nat

object SourceMethod {
  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (stmt: parser.MethodDeclarationStmt, location: SourceLocation)
  (desc: MethodDescriptor)
  (methodOwner: ArMethod.Owner[context2.type, DeclarationPayloadSpecifier])
  : context2.Comp[ArMethod[context2.type, DeclarationPayloadSpecifier]] = {
    import context2._
    
    for {
      sigCache <- Compilation[Comp].createCache[context2.signatureContext.Signature[FunctionResultInfo, _ <: Nat]]
      implCache <- Compilation[Comp].createCache[context2.TMethodImplementation]
    } yield new ArMethod[context2.type, DeclarationPayloadSpecifier] {
      override val context: context2.type = context2
      override val contextProof: context.type Is context2.type = Is.refl

      import context.scopeContext.ScopeExtensions

      override val descriptor: MethodDescriptor = desc
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


      override val owner: ArMethod.Owner[context.type, DeclarationPayloadSpecifier] = methodOwner

      override lazy val signatureUnsubstituted: Comp[context.signatureContext.Signature[FunctionResultInfo, _ <: Nat]] =
        sigCache(
          SourceSignatureCreator.fromParameters[FunctionResultInfo](context2)(
            env(context)(effectInfo, descriptor)
          )(descriptor)(stmt.parameters)(resultCreator(stmt.returnType))
        )



      override lazy val payload: Comp[context.TMethodImplementation] =
        implCache(
          if(isAbstract)
            context.abstractMethodImplementation.pure[Comp]
          else
            stmt.body match {
              case Some(WithSource(parser.ExternExpr(specifier), location)) =>
                context.createExternMethodImplementation(specifier, CompilationMessageSource.SourceFile(env.fileSpec, location))

              case Some(body) =>
                for {
                  sig <- signatureUnsubstituted
                  env2 = env(context)(effectInfo, descriptor)
                  env3 = env2.copy(scope = env2.scope.addParameters(
                    sig.unsubstitutedParameters
                  ))
                  expr <- ExpressionConverter.convertExpression(context)(env3)(sig.unsubstitutedResult.returnType)(body)
                } yield context.createExprMethodImplementation(expr)

              case None =>
                Compilation[Comp].forErrors(CompilationError.NonAbstractMethodNotImplemented(descriptor, CompilationMessageSource.SourceFile(env.fileSpec, location)))
            }
        )


      private def resultCreator(returnTypeExpr: WithSource[parser.Expr]): ResultCreator.Aux[context2.type, FunctionResultInfo] =  new ResultCreator[FunctionResultInfo] {

        override val context: context2.type = context2

        override def createResult
        (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
        : context.Comp[FunctionResultInfo[context.type, context.typeSystem.type]] = {
          import context._
          ExpressionConverter.convertTypeExpression(context)(env)(returnTypeExpr)
            .map { t => FunctionResultInfo(context.typeSystem)(t) }
        }
      }

    }
  }

}
