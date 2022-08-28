package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.expr.*
import dev.argon.util.{*, given}
import dev.argon.parser.IdentifierExpr

import java.util.Objects

abstract class ArgonExprContext extends ExprContext with UsingContext {
  override type TClass = ArClass
  override type TTrait = ArTrait
  override type TFunction = ArFunc
  override type TMethod = ArMethod
  override type TClassConstructor = ClassConstructor
  override type TVariable = Variable
  override type TLocalVariable = LocalVariable

  override def classCanEqual: CanEqual[TClass, TClass] = summon[CanEqual[TClass, TClass]]
  override def traitCanEqual: CanEqual[TTrait, TTrait] = summon[CanEqual[TTrait, TTrait]]

  override def functionCanEqual: CanEqual[TFunction, TFunction] = summon[CanEqual[TFunction, TFunction]]
  override def methodCanEqual: CanEqual[TMethod, TMethod] = summon[CanEqual[TMethod, TMethod]]

  override def classConstructorCanEqual: CanEqual[TClassConstructor, TClassConstructor] =
    summon[CanEqual[TClassConstructor, TClassConstructor]]

  override def variableCanEqual: CanEqual[TVariable, TVariable] = summon[CanEqual[TVariable, TVariable]]

  override def localVariableCanEqual: CanEqual[TLocalVariable, TLocalVariable] =
    summon[CanEqual[TLocalVariable, TLocalVariable]]

  sealed abstract class Variable derives CanEqual {
    val varType: WrapExpr
    def name: Option[IdentifierExpr]
    def isMutable: Boolean
    def isErased: Boolean
  }

  final class LocalVariable
    (
      val id: UniqueIdentifier,
      override val varType: WrapExpr,
      override val name: Option[IdentifierExpr],
      override val isMutable: Boolean,
      override val isErased: Boolean,
    ) extends Variable {

    override def equals(obj: Any): Boolean =
      obj.asInstanceOf[Matchable] match {
        case other: LocalVariable => other.id == id
        case _ => false
      }

    override def hashCode(): Int = id.hashCode()

    override def toString: String = s"LocalVariable(id=$id, name=$name, type=$varType)"
  }

  final class InstanceVariable
  (
    val method: ArMethod,
    override val varType: WrapExpr,
    override val name: Option[IdentifierExpr],
  ) extends Variable {

    override def isMutable: Boolean = false
    override def isErased: Boolean = false

    override def equals(obj: Any): Boolean =
      obj.asInstanceOf[Matchable] match {
        case other: InstanceVariable => other.method == method
        case _ => false
      }

    override def hashCode(): Int = method.hashCode()
  }

  final class MemberVariable
  (
    val ownerClass: ArClass,
    override val varType: WrapExpr,
    override val name: Some[IdentifierExpr],
    override val isMutable: Boolean,
  ) extends Variable {

    override def isErased: Boolean = false

    override def equals(obj: Any): Boolean =
      obj.asInstanceOf[Matchable] match {
        case other: MemberVariable => other.ownerClass == ownerClass && other.name == name
        case _ => false
      }

    override def hashCode(): Int =
      Objects.hash(ownerClass, name)
  }

  type ParameterVariableOwner = ParameterVariableOwnerC[context.type]

  final class ParameterVariable
    (
      val owner: ParameterVariableOwner,
      val parameterIndex: Int,
      override val varType: WrapExpr,
      override val isErased: Boolean,
      override val name: Option[IdentifierExpr],
    ) extends Variable {
    override def isMutable: Boolean = false

    override def equals(obj: Any): Boolean =
      obj.asInstanceOf[Matchable] match {
        case other: ParameterVariable => parameterIndex == other.parameterIndex && other.owner.equals(owner)
        case _ => false
      }

    override def hashCode(): Int = owner.hashCode() + 3 * parameterIndex.hashCode()

    override def toString: String =
      s"ParameterVariable($name)"
  }

  final class FunctionResultVariable
  (
    val owner: ParameterVariableOwner,
    override val varType: WrapExpr,
  ) extends Variable {

    override def name: Option[IdentifierExpr] = Some(IdentifierExpr.FunctionResultValue)
    override def isMutable: Boolean = false
    override def isErased: Boolean = true


    override def equals(obj: Any): Boolean =
      obj.asInstanceOf[Matchable] match {
        case other: FunctionResultVariable => other.owner.equals(owner)
        case _ => false
      }

    override def hashCode(): Int = owner.hashCode()

    override def toString: String =
      s"ParameterVariable($name)"
  }


  final case class ClassResult
  (
    classTypeSuperType: WrapExpr,
    baseClass: Comp[Option[ArExpr[ExprConstructor.ClassType]]],
    baseTraits: Comp[Seq[ArExpr[ExprConstructor.TraitType]]],
  )

  final case class TraitResult
  (
    traitTypeSuperType: WrapExpr,
    baseTraits: Comp[Seq[ArExpr[ExprConstructor.TraitType]]],
  )

  final case class FunctionResult
  (
    returnType: WrapExpr,
    ensuresClauses: Seq[WrapExpr],
  )

}

object ArgonExprContext {


}

type ParameterVariableOwnerC[TContext <: Context] =
  ArMethodC & HasContext[TContext] |
    ArFuncC & HasContext[TContext] |
    ArClassC & HasContext[TContext] |
    ArTraitC & HasContext[TContext] |
    ClassConstructorC & HasContext[TContext]
