package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import com.mi3software.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import com.mi3software.argon.parser
import com.mi3software.argon.util._
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier

object SourceMethod {
  def apply[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (env: EnvCreator[context2.type])
  (stmt: parser.MethodDeclarationStmt, location: SourceLocation)
  (desc: MethodDescriptor)
  (methodOwner: ArMethod.Owner[context2.type, DeclarationPayloadSpecifier])
  : TComp[ArMethod[context2.type, DeclarationPayloadSpecifier]] = for {
    sigCache <- Compilation[TComp].createCache[context2.signatureContext.Signature[FunctionResultInfo]]
    implCache <- Compilation[TComp].createCache[context2.TMethodImplementation]
  } yield new ArMethod[context2.type, DeclarationPayloadSpecifier] {
    override val context: context2.type = context2
    override val contextProof: Leibniz[context.type, context2.type, context.type, context2.type] = Leibniz.refl

    import context.scopeContext.ScopeExtensions

    override val descriptor: MethodDescriptor = desc


    override val isVirtual: Boolean = stmt.modifiers.exists {
      case WithSource(parser.VirtualModifier, _) => true
      case _ => false
    }

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

    override val effectInfo: EffectInfo = EffectInfo(stmt.purity)


    override val owner: ArMethod.Owner[context.type, DeclarationPayloadSpecifier] = methodOwner

    override lazy val signature: TComp[context.signatureContext.Signature[FunctionResultInfo]] =
      sigCache(
        SourceSignatureCreator.fromParameters[TComp, FunctionResultInfo](context2)(
          env(context)(effectInfo, descriptor)
        )(descriptor)(stmt.parameters)(resultCreator(stmt.returnType))
      )



    override lazy val payload: TComp[context.TMethodImplementation] =
      implCache(
        if(isAbstract)
          context.abstractMethodImplementation.point[TComp]
        else
          stmt.body match {
            case Some(WithSource(Vector(WithSource(parser.ExternExpr(specifier), location)), _)) =>
              context.createExternMethodImplementation(specifier, CompilationMessageSource.SourceFile(env.fileSpec, location))

            case Some(body) =>
              for {
                sig <- signature
                env2 = env(context)(effectInfo, descriptor)
                env3 = env2.copy(scope = env2.scope.addVariables(
                  sig.unsubstitutedParameters.flatMap(_.tupleVars)
                ))
                expr <- ExpressionConverter.convertStatementList(context)(env3)(sig.unsubstitutedResult.returnType)(body)
              } yield context.createExprMethodImplementation(expr)

            case None =>
              Compilation[TComp].forErrors(CompilationError.NonAbstractMethodNotImplemented(descriptor, CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
      )
  }

  def resultCreator(returnTypeExpr: WithSource[parser.Expr]): ResultCreator[FunctionResultInfo] =  new ResultCreator[FunctionResultInfo] {
    override def createResult[TComp[+ _] : Compilation]
    (context: ContextComp[TComp])
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : TComp[FunctionResultInfo[context.type, context.typeSystem.type]] =
      ExpressionConverter.convertTypeExpression(context)(env)(returnTypeExpr)
        .map { t => FunctionResultInfo(context.typeSystem)(t) }
  }

}
