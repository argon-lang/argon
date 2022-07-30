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

  sealed trait Variable derives CanEqual {
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
    ) extends Variable {

    override def isErased: Boolean = false

    override def equals(obj: Any): Boolean =
      obj match {
        case other: LocalVariable => other.id == id
        case _ => false
      }

    override def hashCode(): Int = id.hashCode()
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
      obj match {
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
  ) extends Variable {

    override def isMutable: Boolean = false
    override def isErased: Boolean = false

    override def equals(obj: Any): Boolean =
      obj match {
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
    ) extends Variable {

    override def name: Option[IdentifierExpr] = None
    override def isMutable: Boolean = false

    override def equals(obj: Any): Boolean =
      obj match {
        case other: ParameterVariable => parameterIndex == other.parameterIndex && other.owner.equals(owner)
        case _ => false
      }

    override def hashCode(): Int = owner.hashCode() + 3 * parameterIndex.hashCode()
  }

}

object ArgonExprContext {

  def convertWrapExpr[F[+_]: Monad]
    (
      context: Context
    )
    (
      ec1: ArgonExprContext with HasContext[context.type],
      ec2: ArgonExprContext with HasContext[context.type],
    )
    (
      f: ec1.THole => F[ec2.WrapExpr]
    )
    (
      e: ec1.WrapExpr
    )
    : F[ec2.WrapExpr] =
    e match {
      case ec1.WrapExpr.OfExpr(expr) => convertArExpr(context)(ec1, ec2)(f)(expr).map(ec2.WrapExpr.OfExpr.apply)
      case ec1.WrapExpr.OfHole(hole) => f(hole)
    }

  def convertArExpr[F[+_]: Monad]
    (
      context: Context
    )
    (
      ec1: ArgonExprContext with HasContext[context.type],
      ec2: ArgonExprContext with HasContext[context.type],
    )
    (
      f: ec1.THole => F[ec2.WrapExpr]
    )
    (
      e: ec1.ArExpr[ec1.ExprConstructor]
    )
    : F[ec2.ArExpr[ec2.ExprConstructor]] =
    def convertClassConstructorCallArgs
      (ctor: ec2.ExprConstructor { type ConstructorArgs = ec2.ExprConstructor.ClassConstructorCallArgs })
      (args: ec1.ExprConstructor.ClassConstructorCallArgs)
      : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (classType, ctorArgs) = args
      for {
        classType2 <- convertClassType(context)(ec1, ec2)(f)(classType)
        ctorArgs2 <- Traverse[Vector].traverse(ctorArgs)(convertWrapExpr(context)(ec1, ec2)(f))
      } yield ec2.ArExpr(ctor, (classType2, ctorArgs2))
    end convertClassConstructorCallArgs

    def convertVectorArgs(ctor: ec2.ExprConstructor { type ConstructorArgs = Vector[ec2.WrapExpr] })
      (args: Vector[ec1.WrapExpr])
      : F[ec2.ArExpr[ec2.ExprConstructor]] =
      for {
        args2 <- Traverse[Vector].traverse(args)(convertWrapExpr(context)(ec1, ec2)(f))
      } yield ec2.ArExpr(ctor, args2)

    def convertNonEmptyListArgs(ctor: ec2.ExprConstructor { type ConstructorArgs = NonEmptyList[ec2.WrapExpr] })
      (args: NonEmptyList[ec1.WrapExpr])
      : F[ec2.ArExpr[ec2.ExprConstructor]] =
      for {
        args2 <- Traverse[NonEmptyList].traverse(args)(convertWrapExpr(context)(ec1, ec2)(f))
      } yield ec2.ArExpr(ctor, args2)

    def convertMethodCallArgs(ctor: ec2.ExprConstructor { type ConstructorArgs = ec2.ExprConstructor.MethodCallArgs })
      (args: ec1.ExprConstructor.MethodCallArgs)
      : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (instance, callArgs) = args
      for {
        instance2 <- convertWrapExpr(context)(ec1, ec2)(f)(instance)
        callArgs2 <- Traverse[Vector].traverse(callArgs)(convertWrapExpr(context)(ec1, ec2)(f))
      } yield ec2.ArExpr(ctor, (instance2, callArgs2))
    end convertMethodCallArgs

    def convertPatternMatchArgs[N <: Nat]
      (ctor: ec2.ExprConstructor { type ConstructorArgs = ec2.ExprConstructor.PatternMatchArgs[N] })
      (args: ec1.ExprConstructor.PatternMatchArgs[N])
      : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (obj, cases) = args
      for {
        obj2 <- convertWrapExpr(context)(ec1, ec2)(f)(obj)
        cases2 <- Traverse[[C] =>> NList[N, C]].traverse(cases)(convertWrapExpr(context)(ec1, ec2)(f))
      } yield ec2.ArExpr(ctor, (obj2, cases2))
    end convertPatternMatchArgs

    def convertExprArgs(ctor: ec2.ExprConstructor { type ConstructorArgs = ec2.WrapExpr })(args: ec1.WrapExpr)
      : F[ec2.ArExpr[ec2.ExprConstructor]] =
      for {
        args2 <- convertWrapExpr(context)(ec1, ec2)(f)(args)
      } yield ec2.ArExpr(ctor, args2)

    def convertPairArgs(ctor: ec2.ExprConstructor { type ConstructorArgs = (ec2.WrapExpr, ec2.WrapExpr) })
      (args: (ec1.WrapExpr, ec1.WrapExpr))
      : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (a1, b1) = args
      for {
        a2 <- convertWrapExpr(context)(ec1, ec2)(f)(a1)
        b2 <- convertWrapExpr(context)(ec1, ec2)(f)(b1)
      } yield ec2.ArExpr(ctor, (a2, b2))
    end convertPairArgs

    def convertTuple3Args
      (ctor: ec2.ExprConstructor { type ConstructorArgs = (ec2.WrapExpr, ec2.WrapExpr, ec2.WrapExpr) })
      (args: (ec1.WrapExpr, ec1.WrapExpr, ec1.WrapExpr))
      : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (a1, b1, c1) = args
      for {
        a2 <- convertWrapExpr(context)(ec1, ec2)(f)(a1)
        b2 <- convertWrapExpr(context)(ec1, ec2)(f)(b1)
        c2 <- convertWrapExpr(context)(ec1, ec2)(f)(c1)
      } yield ec2.ArExpr(ctor, (a2, b2, c2))
    end convertTuple3Args

    (e.constructor: e.constructor.type & ec1.ExprConstructor) match {
      case ctor: (e.constructor.type & ec1.ExprConstructor.BindVariable) =>
        convertLocalVariable(context)(ec1, ec2)(f)(ctor.variable).flatMap { var2 =>
          convertExprArgs(ec2.ExprConstructor.BindVariable(var2))(e.getArgs(ctor))
        }

      case ctor: (e.constructor.type & ec1.ExprConstructor.ClassConstructorCall) =>
        convertClassConstructorCallArgs(ec2.ExprConstructor.ClassConstructorCall(ctor.classCtor))(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.EnsureExecuted.type) =>
        convertPairArgs(ec2.ExprConstructor.EnsureExecuted)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.FunctionCall) =>
        convertVectorArgs(ec2.ExprConstructor.FunctionCall(ctor.function))(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.FunctionObjectCall.type) =>
        convertPairArgs(ec2.ExprConstructor.FunctionObjectCall)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.IfElse.type) =>
        convertTuple3Args(ec2.ExprConstructor.IfElse)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.LoadConstantBool) =>
        Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.LoadConstantBool(ctor.b), EmptyTuple))

      case ctor: (e.constructor.type & ec1.ExprConstructor.LoadConstantInt) =>
        Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.LoadConstantInt(ctor.i), EmptyTuple))

      case ctor: (e.constructor.type & ec1.ExprConstructor.LoadConstantString) =>
        Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.LoadConstantString(ctor.s), EmptyTuple))

      case ctor: (e.constructor.type & ec1.ExprConstructor.LoadLambda) =>
        convertLocalVariable(context)(ec1, ec2)(f)(ctor.argVariable).flatMap { var2 =>
          convertExprArgs(ec2.ExprConstructor.LoadLambda(var2))(e.getArgs(ctor))
        }

      case ctor: (e.constructor.type & ec1.ExprConstructor.LoadTuple.type) =>
        convertVectorArgs(ec2.ExprConstructor.LoadTuple)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.LoadTupleElement) =>
        convertExprArgs(ec2.ExprConstructor.LoadTupleElement(ctor.index))(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.LoadVariable) =>
        convertVariable(context)(ec1, ec2)(f)(ctor.variable).map { var2 =>
          ec2.ArExpr(ec2.ExprConstructor.LoadVariable(var2), EmptyTuple)
        }

      case ctor: (e.constructor.type & ec1.ExprConstructor.MethodCall) =>
        convertMethodCallArgs(ec2.ExprConstructor.MethodCall(ctor.method))(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.PatternMatch[n]) =>
        ctor.patterns.traverse(convertPatternExpr(context)(ec1, ec2)(f)).flatMap { patterns2 =>
          convertPatternMatchArgs[n](ec2.ExprConstructor.PatternMatch(patterns2))(e.getArgs(ctor))
        }

      case ctor: (e.constructor.type & ec1.ExprConstructor.RaiseException.type) =>
        convertExprArgs(ec2.ExprConstructor.RaiseException)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.Sequence.type) =>
        convertNonEmptyListArgs(ec2.ExprConstructor.Sequence)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.StoreVariable) =>
        convertVariable(context)(ec1, ec2)(f)(ctor.variable).flatMap { var2 =>
          convertExprArgs(ec2.ExprConstructor.StoreVariable(var2))(e.getArgs(ctor))
        }

      case ctor: (e.constructor.type & ec1.ExprConstructor.TypeN.type) =>
        convertExprArgs(ec2.ExprConstructor.TypeN)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.OmegaTypeN) =>
        Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.OmegaTypeN(ctor.level), EmptyTuple))

      case ctor: (e.constructor.type & ec1.ExprConstructor.AnyType.type) =>
        Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.AnyType, EmptyTuple))

      case ctor: (e.constructor.type & ec1.ExprConstructor.TraitType) =>
        convertVectorArgs(ec2.ExprConstructor.TraitType(ctor.arTrait))(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.ClassType) =>
        convertVectorArgs(ec2.ExprConstructor.ClassType(ctor.arClass))(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.FunctionType.type) =>
        convertPairArgs(ec2.ExprConstructor.FunctionType)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.UnionType.type) =>
        convertPairArgs(ec2.ExprConstructor.UnionType)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.IntersectionType.type) =>
        convertPairArgs(ec2.ExprConstructor.IntersectionType)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.ExistentialType) =>
        convertLocalVariable(context)(ec1, ec2)(f)(ctor.variable).flatMap { var2 =>
          convertExprArgs(ec2.ExprConstructor.ExistentialType(var2))(e.getArgs(ctor))
        }

      case ctor: (e.constructor.type & ec1.ExprConstructor.ConjunctionType.type) =>
        convertPairArgs(ec2.ExprConstructor.ConjunctionType)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.DisjunctionType.type) =>
        convertPairArgs(ec2.ExprConstructor.DisjunctionType)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.NeverType.type) =>
        Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.NeverType, EmptyTuple))

      case ctor: (e.constructor.type & ec1.ExprConstructor.SubtypeWitnessType.type) =>
        convertPairArgs(ec2.ExprConstructor.SubtypeWitnessType)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.EqualTo.type) =>
        convertPairArgs(ec2.ExprConstructor.EqualTo)(e.getArgs(ctor))

      case ctor: (e.constructor.type & ec1.ExprConstructor.AssumeErasedValue.type) =>
        Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.AssumeErasedValue, EmptyTuple))

    }
  end convertArExpr

  def convertClassType[F[+_]: Monad]
    (
      context: Context
    )
    (
      ec1: ArgonExprContext with HasContext[context.type],
      ec2: ArgonExprContext with HasContext[context.type],
    )
    (
      f: ec1.THole => F[ec2.WrapExpr]
    )
    (
      e: ec1.ArExpr[ec1.ExprConstructor.ClassType]
    )
    : F[ec2.ArExpr[ec2.ExprConstructor.ClassType]] =
    for {
      args2 <- Traverse[Vector].traverse(e.args)(convertWrapExpr(context)(ec1, ec2)(f))
    } yield ec2.ArExpr(ec2.ExprConstructor.ClassType(e.constructor.arClass), args2)

  def convertTraitType[F[+_]: Monad]
    (
      context: Context
    )
    (
      ec1: ArgonExprContext with HasContext[context.type],
      ec2: ArgonExprContext with HasContext[context.type],
    )
    (
      f: ec1.THole => F[ec2.WrapExpr]
    )
    (
      e: ec1.ArExpr[ec1.ExprConstructor.TraitType]
    )
    : F[ec2.ArExpr[ec2.ExprConstructor.TraitType]] =
    for {
      args2 <- Traverse[Vector].traverse(e.args)(convertWrapExpr(context)(ec1, ec2)(f))
    } yield ec2.ArExpr(ec2.ExprConstructor.TraitType(e.constructor.arTrait), args2)

  def convertVariable[F[+_]: Monad]
    (
      context: Context
    )
    (
      ec1: ArgonExprContext with HasContext[context.type],
      ec2: ArgonExprContext with HasContext[context.type],
    )
    (
      f: ec1.THole => F[ec2.WrapExpr]
    )
    (
      variable: ec1.Variable
    )
    : F[ec2.Variable] =
    variable match {
      case variable: ec1.LocalVariable =>
        convertLocalVariable(context)(ec1, ec2)(f)(variable)

      case variable: ec1.InstanceVariable =>
        for {
          varType2 <- convertWrapExpr(context)(ec1, ec2)(f)(variable.varType)
        } yield ec2.InstanceVariable(variable.method, varType2, variable.name)

      case variable: ec1.MemberVariable =>
        for {
          varType2 <- convertWrapExpr(context)(ec1, ec2)(f)(variable.varType)
        } yield ec2.MemberVariable(variable.ownerClass, varType2, variable.name)

      case variable: ec1.ParameterVariable =>
        for {
          varType2 <- convertWrapExpr(context)(ec1, ec2)(f)(variable.varType)
        } yield ec2.ParameterVariable(variable.owner, variable.parameterIndex, varType2, variable.isErased)
    }

  def convertLocalVariable[F[+_]: Monad]
    (
      context: Context
    )
    (
      ec1: ArgonExprContext with HasContext[context.type],
      ec2: ArgonExprContext with HasContext[context.type],
    )
    (
      f: ec1.THole => F[ec2.WrapExpr]
    )
    (
      variable: ec1.LocalVariable
    )
    : F[ec2.LocalVariable] =
    for {
      varType2 <- convertWrapExpr(context)(ec1, ec2)(f)(variable.varType)
    } yield ec2.LocalVariable(variable.id, varType2, variable.name, variable.isMutable)

  private def convertPatternExpr[F[+_]: Monad]
    (
      context: Context
    )
    (
      ec1: ArgonExprContext with HasContext[context.type],
      ec2: ArgonExprContext with HasContext[context.type],
    )
    (
      f: ec1.THole => F[ec2.WrapExpr]
    )
    (
      e: ec1.PatternExpr
    )
    : F[ec2.PatternExpr] =
    e match {
      case ec1.PatternExpr.Binding(variable) =>
        convertLocalVariable(context)(ec1, ec2)(f)(variable).map(ec2.PatternExpr.Binding.apply)
      case ec1.PatternExpr.CastBinding(variable) =>
        convertLocalVariable(context)(ec1, ec2)(f)(variable).map(ec2.PatternExpr.CastBinding.apply)
    }

}

type ParameterVariableOwnerC[TContext <: Context] =
  ArMethodC with HasContext[TContext] |
    ArFuncC with HasContext[TContext] |
    ArClassC with HasContext[TContext] |
    ArTraitC with HasContext[TContext] |
    ClassConstructorC with HasContext[TContext]
