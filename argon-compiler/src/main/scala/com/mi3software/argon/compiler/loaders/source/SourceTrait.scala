package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler.core.PayloadSpecifiers._
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import com.mi3software.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.parser
import com.mi3software.argon.parser.TraitDeclarationStmt
import com.mi3software.argon.util.{SourceLocation, WithSource}
import scalaz._
import Scalaz._

private[compiler] object SourceTrait {

  final case class GroupedStaticStatements
  (
    staticMethods: Vector[WithSource[parser.MethodDeclarationStmt]],
  )

  final case class GroupedInstanceStatements
  (
    methods: Vector[WithSource[parser.MethodDeclarationStmt]],
    fields: Vector[WithSource[parser.FieldDeclarationStmt]],
  )

  def apply[TComp[+_] : Monad : Compilation]
  (context2: ContextComp[TComp])
  (env: EnvCreator[context2.type])
  (stmt: TraitDeclarationStmt)
  (desc: TraitDescriptor)
  : TComp[ArTrait[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] { val descriptor: desc.type }] = for {
    sigCache <- Compilation[TComp].createCache[context2.signatureContext.Signature[ArTrait.ResultInfo]]

    groupedStaticCache <- Compilation[TComp].createCache[GroupedStaticStatements]
    groupedInstCache <- Compilation[TComp].createCache[GroupedInstanceStatements]

    methodCache <- Compilation[TComp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
    staticMethodCache <- Compilation[TComp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]

  } yield new ArTrait[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
    override val context: context2.type = context2

    import context.signatureContext.Signature

    override val contextProof: Leibniz[context.type, context2.type, context.type, context2.type] = Leibniz.refl

    override val descriptor: desc.type = desc

    override val isSealed: Boolean = stmt.modifiers.exists {
      case WithSource(parser.SealedModifier, _) => true
      case _ => false
    }

    override lazy val signature: TComp[Signature[ArTrait.ResultInfo]] =
      sigCache(
        SourceSignatureCreator.fromParameters[TComp, ArTrait.ResultInfo](context2)(
          env(context)(EffectInfo.pure, descriptor)
        )(descriptor)(stmt.parameters)(resultCreator(stmt.baseType))
      )

    override lazy val methods: TComp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] = ??? : TComp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
    override lazy val staticMethods: TComp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] = ??? : TComp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]

    override val payload: Unit = ()


  }


  private def resultCreator(baseTypeExpr: Option[WithSource[parser.Expr]]): ResultCreator[ArTrait.ResultInfo] = new ResultCreator[ArTrait.ResultInfo] {
    override def createResult[TComp[+ _] : Compilation]
    (context: ContextComp[TComp])
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : TComp[ArTrait.ResultInfo[context.type, context.typeSystem.type]] =
      (baseTypeExpr match {
        case Some(baseTypeExpr) =>
          ExpressionConverter.convertTypeExpression(context)(env)(baseTypeExpr)
            .flatMap(typeToBaseTypes(context)(env)(_)(baseTypeExpr.location)(context.typeSystem.BaseTypeInfoTrait(Vector())))
        case None =>
          context.typeSystem.BaseTypeInfoTrait(Vector()).point[TComp]
      })
        .map { baseTypes => ArTrait.ResultInfo(context.typeSystem)(baseTypes) }
  }

  private def typeToBaseTypes[TComp[+ _] : Compilation]
  (context: ContextComp[TComp])
  (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
  (t: context.typeSystem.TType)
  (location: SourceLocation)
  (acc: context.typeSystem.BaseTypeInfoTrait)
  : TComp[context.typeSystem.BaseTypeInfoTrait] =
    t match {
      case t: context.typeSystem.TraitType =>
        acc.copy(baseTraits = acc.baseTraits :+ t).point[TComp]

      case context.typeSystem.IntersectionType(first, second) =>
        typeToBaseTypes(context)(env)(first)(location)(acc).flatMap { acc2 =>
          typeToBaseTypes(context)(env)(second)(location)(acc2)
        }

      case _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidBaseType(CompilationMessageSource.SourceFile(env.fileSpec, location)))
    }
}
