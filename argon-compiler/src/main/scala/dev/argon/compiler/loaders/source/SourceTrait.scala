package dev.argon.compiler.loaders.source

import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.compiler.lookup._
import dev.argon.parser
import dev.argon.parser.TraitDeclarationStmt
import dev.argon.util.{SourceLocation, WithSource}
import scalaz._
import Scalaz._

private[compiler] object SourceTrait extends AccessModifierHelpers {

  final case class GroupedStaticStatements
  (
    staticMethods: Vector[WithSource[parser.MethodDeclarationStmt]],
  )

  final case class GroupedInstanceStatements
  (
    methods: Vector[WithSource[parser.MethodDeclarationStmt]],
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

    private val groupedStatic =
      groupedStaticCache(
        stmt.body.toVector.foldLeftM(GroupedStaticStatements(Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
            group.copy(staticMethods = group.staticMethods :+ WithSource(stmt, location)).point[TComp]

          case (_, WithSource(_, location)) =>
            Compilation[TComp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
        }
      )

    private val groupedInst =
      groupedInstCache(
        stmt.instanceBody.foldLeftM(GroupedInstanceStatements(Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
            group.copy(methods = group.methods :+ WithSource(stmt, location)).point[TComp]

          case (_, WithSource(_, location)) =>
            Compilation[TComp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
        }
      )



    override val methods: TComp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
      methodCache(groupedInst.flatMap { inst =>
        inst.methods.zipWithIndex.traverse { case (method, i) =>
          parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
            val memberName = method.value.name match {
              case Some(name) => MemberName.Normal(name)
              case None => MemberName.Unnamed
            }

            val desc = MethodDescriptor(descriptor, i, memberName)
            SourceMethod(context)(env)(method.value, method.location)(desc)(ArMethod.TraitOwner(this))
              .map(MethodBinding(memberName, i, modifiers, _))
          }
        }
      })

    override val staticMethods: TComp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
      staticMethodCache(groupedStatic.flatMap { statics =>
        statics.staticMethods.zipWithIndex.traverse { case (method, i) =>
          parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
            val memberName = method.value.name match {
              case Some(name) => MemberName.Normal(name)
              case None => MemberName.Unnamed
            }

            val desc = MethodDescriptor(TraitObjectDescriptor(descriptor), i, memberName)
            SourceMethod(context)(env)(method.value, method.location)(desc)(ArMethod.TraitObjectOwner(this))
              .map(MethodBinding(memberName, i, modifiers, _))
          }
        }
      })
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
