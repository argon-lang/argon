package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.util.{*, given}

abstract class ExprProcessor[F[+_]: Monad] extends UsingContext {
  val ec1: ArgonExprContext & HasContext[context.type]
  val ec2: ArgonExprContext & HasContext[context.type]

  
  def processHole(hole: ec1.THole): F[ec2.WrapExpr]
  

  def processWrapExpr(e: ec1.WrapExpr): F[ec2.WrapExpr] =
    e match {
      case ec1.WrapExpr.OfExpr(expr) => processArExpr(expr)
      case ec1.WrapExpr.OfHole(hole) => processHole(hole)
    }

  def processArExpr(e: ec1.ArExpr[ec1.ExprConstructor]): F[ec2.WrapExpr] =
    def processSeqArgs(ctor: ec2.ExprConstructor {type ConstructorArgs = Seq[ec2.WrapExpr]})
                      (args: Seq[ec1.WrapExpr])
    : F[ec2.ArExpr[ec2.ExprConstructor]] =
      for {
        args2 <- Traverse[Seq].traverse(args)(processWrapExpr)
      } yield ec2.ArExpr(ctor, args2)

    def processNonEmptyListArgs(ctor: ec2.ExprConstructor {type ConstructorArgs = NonEmptyList[ec2.WrapExpr]})
                               (args: NonEmptyList[ec1.WrapExpr])
    : F[ec2.ArExpr[ec2.ExprConstructor]] =
      for {
        args2 <- Traverse[NonEmptyList].traverse(args)(processWrapExpr)
      } yield ec2.ArExpr(ctor, args2)

    def processMethodCallArgs(ctor: ec2.ExprConstructor {type ConstructorArgs = ec2.ExprConstructor.MethodCallArgs})
                             (args: ec1.ExprConstructor.MethodCallArgs)
    : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (instance, ownerType, callArgs) = args
      for {
        instance2 <- processWrapExpr(instance)
        ownerType2 <- ownerType match {
          case ec1.ExprConstructor.MethodCallOwnerType.OwnedByClass(classType) =>
            processClassType(classType)
              .map(ec2.ExprConstructor.MethodCallOwnerType.OwnedByClass.apply)

          case ec1.ExprConstructor.MethodCallOwnerType.OwnedByTrait(traitType) =>
            processTraitType(traitType)
              .map(ec2.ExprConstructor.MethodCallOwnerType.OwnedByTrait.apply)
        }
        callArgs2 <- Traverse[Seq].traverse(callArgs)(processWrapExpr)
      } yield ec2.ArExpr(ctor, (instance2, ownerType2, callArgs2))
    end processMethodCallArgs

    def processPatternMatchArgs[N <: Nat]
    (ctor: ec2.ExprConstructor {type ConstructorArgs = ec2.ExprConstructor.PatternMatchArgs[N]})
    (args: ec1.ExprConstructor.PatternMatchArgs[N])
    : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (obj, cases) = args
      for {
        obj2 <- processWrapExpr(obj)
        cases2 <- Traverse[[C] =>> NList[N, C]].traverse(cases)(processWrapExpr)
      } yield ec2.ArExpr(ctor, (obj2, cases2))
    end processPatternMatchArgs

    def processExprArgs(ctor: ec2.ExprConstructor {type ConstructorArgs = ec2.WrapExpr})(args: ec1.WrapExpr)
    : F[ec2.ArExpr[ec2.ExprConstructor]] =
      for {
        args2 <- processWrapExpr(args)
      } yield ec2.ArExpr(ctor, args2)

    def processPairArgs(ctor: ec2.ExprConstructor {type ConstructorArgs = (ec2.WrapExpr, ec2.WrapExpr)})
                       (args: (ec1.WrapExpr, ec1.WrapExpr))
    : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (a1, b1) = args
      for {
        a2 <- processWrapExpr(a1)
        b2 <- processWrapExpr(b1)
      } yield ec2.ArExpr(ctor, (a2, b2))
    end processPairArgs

    def processTuple3Args
    (ctor: ec2.ExprConstructor {type ConstructorArgs = (ec2.WrapExpr, ec2.WrapExpr, ec2.WrapExpr)})
    (args: (ec1.WrapExpr, ec1.WrapExpr, ec1.WrapExpr))
    : F[ec2.ArExpr[ec2.ExprConstructor]] =
      val (a1, b1, c1) = args
      for {
        a2 <- processWrapExpr(a1)
        b2 <- processWrapExpr(b1)
        c2 <- processWrapExpr(c1)
      } yield ec2.ArExpr(ctor, (a2, b2, c2))
    end processTuple3Args

    val processedExpr =
      (e.constructor: e.constructor.type & ec1.ExprConstructor) match {
        case ctor: (e.constructor.type & ec1.ExprConstructor.BindVariable) =>
          processLocalVariable(ctor.variable).flatMap { var2 =>
            processExprArgs(ec2.ExprConstructor.BindVariable(var2))(e.getArgs(ctor))
          }

        case ctor: (e.constructor.type & ec1.ExprConstructor.ClassConstructorCall) =>
          processClassConstructorCall(ec1.ArExpr(ctor, e.getArgs(ctor)))

        case ctor: (e.constructor.type & ec1.ExprConstructor.EnsureExecuted.type) =>
          processPairArgs(ec2.ExprConstructor.EnsureExecuted)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.FunctionCall) =>
          processSeqArgs(ec2.ExprConstructor.FunctionCall(ctor.function))(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.FunctionObjectCall.type) =>
          processPairArgs(ec2.ExprConstructor.FunctionObjectCall)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.IfElse.type) =>
          processTuple3Args(ec2.ExprConstructor.IfElse)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.LoadConstantBool) =>
          Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.LoadConstantBool(ctor.b), EmptyTuple))

        case ctor: (e.constructor.type & ec1.ExprConstructor.LoadConstantInt) =>
          Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.LoadConstantInt(ctor.i), EmptyTuple))

        case ctor: (e.constructor.type & ec1.ExprConstructor.LoadConstantString) =>
          Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.LoadConstantString(ctor.s), EmptyTuple))

        case ctor: (e.constructor.type & ec1.ExprConstructor.LoadLambda) =>
          processLocalVariable(ctor.argVariable).flatMap { var2 =>
            processExprArgs(ec2.ExprConstructor.LoadLambda(var2))(e.getArgs(ctor))
          }

        case ctor: (e.constructor.type & ec1.ExprConstructor.LoadTuple.type) =>
          processSeqArgs(ec2.ExprConstructor.LoadTuple)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.LoadTupleElement) =>
          processExprArgs(ec2.ExprConstructor.LoadTupleElement(ctor.index))(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.LoadVariable) =>
          processVariable(ctor.variable).map { var2 =>
            ec2.ArExpr(ec2.ExprConstructor.LoadVariable(var2), EmptyTuple)
          }

        case ctor: (e.constructor.type & ec1.ExprConstructor.MethodCall) =>
          processMethodCallArgs(ec2.ExprConstructor.MethodCall(ctor.method))(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.PatternMatch[n]) =>
          ctor.patterns.traverse(processPatternExpr).flatMap { patterns2 =>
            processPatternMatchArgs[n](ec2.ExprConstructor.PatternMatch(patterns2))(e.getArgs(ctor))
          }

        case ctor: (e.constructor.type & ec1.ExprConstructor.RaiseException.type) =>
          processExprArgs(ec2.ExprConstructor.RaiseException)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.Sequence.type) =>
          processNonEmptyListArgs(ec2.ExprConstructor.Sequence)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.StoreVariable) =>
          processVariable(ctor.variable).flatMap { var2 =>
            processExprArgs(ec2.ExprConstructor.StoreVariable(var2))(e.getArgs(ctor))
          }

        case ctor: (e.constructor.type & ec1.ExprConstructor.TypeN.type) =>
          processExprArgs(ec2.ExprConstructor.TypeN)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.OmegaTypeN) =>
          Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.OmegaTypeN(ctor.level), EmptyTuple))

        case ctor: (e.constructor.type & ec1.ExprConstructor.AnyType.type) =>
          Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.AnyType, EmptyTuple))

        case ctor: (e.constructor.type & ec1.ExprConstructor.TraitType) =>
          processSeqArgs(ec2.ExprConstructor.TraitType(ctor.arTrait))(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.ClassType) =>
          processSeqArgs(ec2.ExprConstructor.ClassType(ctor.arClass))(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.FunctionType.type) =>
          processPairArgs(ec2.ExprConstructor.FunctionType)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.UnionType.type) =>
          processPairArgs(ec2.ExprConstructor.UnionType)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.IntersectionType.type) =>
          processPairArgs(ec2.ExprConstructor.IntersectionType)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.ExistentialType) =>
          processLocalVariable(ctor.variable).flatMap { var2 =>
            processExprArgs(ec2.ExprConstructor.ExistentialType(var2))(e.getArgs(ctor))
          }

        case ctor: (e.constructor.type & ec1.ExprConstructor.ConjunctionType.type) =>
          processPairArgs(ec2.ExprConstructor.ConjunctionType)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.DisjunctionType.type) =>
          processPairArgs(ec2.ExprConstructor.DisjunctionType)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.NeverType.type) =>
          Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.NeverType, EmptyTuple))

        case ctor: (e.constructor.type & ec1.ExprConstructor.SubtypeWitnessType.type) =>
          processPairArgs(ec2.ExprConstructor.SubtypeWitnessType)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.EqualTo.type) =>
          processPairArgs(ec2.ExprConstructor.EqualTo)(e.getArgs(ctor))

        case ctor: (e.constructor.type & ec1.ExprConstructor.AssumeErasedValue.type) =>
          Monad[F].pure(ec2.ArExpr(ec2.ExprConstructor.AssumeErasedValue, EmptyTuple))
      }

    processedExpr.map(ec2.WrapExpr.OfExpr.apply)
  end processArExpr

  def processClassType(e: ec1.ArExpr[ec1.ExprConstructor.ClassType]): F[ec2.ArExpr[ec2.ExprConstructor.ClassType]] =
    for {
      args2 <- Traverse[Seq].traverse(e.args)(processWrapExpr)
    } yield ec2.ArExpr(ec2.ExprConstructor.ClassType(e.constructor.arClass), args2)

  def processTraitType(e: ec1.ArExpr[ec1.ExprConstructor.TraitType]): F[ec2.ArExpr[ec2.ExprConstructor.TraitType]] =
    for {
      args2 <- Traverse[Seq].traverse(e.args)(processWrapExpr)
    } yield ec2.ArExpr(ec2.ExprConstructor.TraitType(e.constructor.arTrait), args2)


  def processClassConstructorCall(e: ec1.ArExpr[ec1.ExprConstructor.ClassConstructorCall]): F[ec2.ArExpr[ec2.ExprConstructor.ClassConstructorCall]] =
    val (classType, ctorArgs) = e.args
    for {
      classType2 <- processClassType(classType)
      ctorArgs2 <- Traverse[Seq].traverse(ctorArgs)(processWrapExpr)
    } yield ec2.ArExpr(ec2.ExprConstructor.ClassConstructorCall(e.constructor.classCtor), (classType2, ctorArgs2))
  end processClassConstructorCall

  def processVariable(variable: ec1.Variable): F[ec2.Variable] =
    variable match {
      case variable: ec1.LocalVariable =>
        processLocalVariable(variable)

      case variable: ec1.InstanceVariable =>
        for {
          varType2 <- processWrapExpr(variable.varType)
        } yield ec2.InstanceVariable(variable.method, varType2, variable.name)

      case variable: ec1.MemberVariable =>
        processMemberVariable(variable)

      case variable: ec1.ParameterVariable =>
        for {
          varType2 <- processWrapExpr(variable.varType)
        } yield ec2.ParameterVariable(variable.owner, variable.parameterIndex, varType2, variable.isErased, variable.name)
    }

  def processLocalVariable(variable: ec1.LocalVariable): F[ec2.LocalVariable] =
    for {
      varType2 <- processWrapExpr(variable.varType)
    } yield ec2.LocalVariable(variable.id, varType2, variable.name, variable.isMutable)

  def processMemberVariable(variable: ec1.MemberVariable): F[ec2.MemberVariable] =
    for {
      varType2 <- processWrapExpr(variable.varType)
    } yield ec2.MemberVariable(
      variable.ownerClass,
      varType2,
      variable.name,
      isMutable = variable.isMutable
    )

  private def processPatternExpr(e: ec1.PatternExpr): F[ec2.PatternExpr] =
    e match {
      case ec1.PatternExpr.Binding(variable) =>
        processLocalVariable(variable).map(ec2.PatternExpr.Binding.apply)
      case ec1.PatternExpr.CastBinding(variable) =>
        processLocalVariable(variable).map(ec2.PatternExpr.CastBinding.apply)
    }

}
