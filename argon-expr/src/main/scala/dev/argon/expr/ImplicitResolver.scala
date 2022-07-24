package dev.argon.expr

import dev.argon.util.{*, given}
import dev.argon.prover.{PrologContext, PrologSyntax, Proof}
import dev.argon.util.UniqueIdentifier
import zio.*
import zio.stream.{Stream, ZStream}

abstract class ImplicitResolver[R, E] {

  val exprContext: ExprContext
  import exprContext.*

  def createHole: ZIO[R, E, THole]

  sealed trait SubClassResult

  object SubClassResult {
    final case class SubClassProof(proof: WrapExpr) extends SubClassResult
    final case class NotSubClassProof(proof: WrapExpr) extends SubClassResult
    case object Unknown extends SubClassResult

    given CanEqual[Unknown.type, Unknown.type] = CanEqual.canEqualAny
    given CanEqual[Unknown.type, SubClassResult] = CanEqual.canEqualAny
    given CanEqual[SubClassResult, Unknown.type] = CanEqual.canEqualAny
  }

  protected def isSubClass(classA: TClass, aArgs: Seq[WrapExpr], classB: TClass, bArgs: Seq[WrapExpr], fuel: Int)
    : ZIO[R, E, SubClassResult]

  protected def isSubTrait(traitA: TTrait, aArgs: Seq[WrapExpr], traitB: TTrait, bArgs: Seq[WrapExpr], fuel: Int)
    : ZIO[R, E, SubClassResult]

  protected def classImplementsTrait
    (classA: TClass, aArgs: Seq[WrapExpr], traitB: TTrait, bArgs: Seq[WrapExpr], fuel: Int)
    : ZIO[R, E, SubClassResult]

  // These relation methods must match the arguments for the expression constructors
  protected def traitRelations(arTrait: TTrait): ZIO[R, E, Seq[ExprRelation]]
  protected def classRelations(arClass: TClass): ZIO[R, E, Seq[ExprRelation]]
  protected def functionRelations(function: TFunction): ZIO[R, E, Seq[ExprRelation]]
  protected def methodRelations(method: TMethod): ZIO[R, E, Seq[ExprRelation]]
  protected def classConstructorRelations(classCtor: TClassConstructor): ZIO[R, E, Seq[ExprRelation]]

  protected def natLessThanFunction: ZIO[R, E, TFunction]
  protected def boolType: ZIO[R, E, WrapExpr]

  protected def invalidExpr: ZIO[R, E, Nothing]
  protected def invalidPredicateExpr: ZIO[R, E, Nothing]

  protected val evaluator: Evaluator[R, E] { val exprContext: ImplicitResolver.this.exprContext.type }

  object ExprPrologSyntax extends PrologSyntax {
    override type TVariable = THole
    override type TConstructor = ExprConstructor
    override type TPredicateFunction = ExprConstructor

    override def variableCanEqual: CanEqual[THole, THole] = summon
    override def constructorCanEqual: CanEqual[ExprConstructor, ExprConstructor] = summon
    override def predicateFunctionCanEqual: CanEqual[ExprConstructor, ExprConstructor] = summon
  }

  enum TCAtomicProof {
    case ExprProof(expr: WrapExpr)
  }

  enum ExprRelation derives CanEqual {
    case SubType, SuperType
    case TypeEquality
    case SyntacticEquality
  }

  private final class TCPrologContext(createdHoles: Ref[Set[THole]]) extends PrologContext[R, E] {
    override val syntax: ExprPrologSyntax.type = ExprPrologSyntax
    import syntax.*
    override type ProofAtom = TCAtomicProof

    override type TRelation = ExprRelation
    override type TConstraints = ExprConstraints[Expr]

    protected override def normalize(expr: Expr, fuel: Int): ZIO[R, E, Expr] =
      exprToWrapExpr(expr).flatMap { expr =>
        evaluator.normalizeTopLevelWrap(expr, fuel)
          .map(wrapExprToExpr)
      }

    private def builtinAssertions: ZIO[R, E, List[(Proof[ProofAtom], Predicate)]] =
      ZIO.collectAll(List(
        // A <: A
        (for {
          a <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(a)))
        )),

        // A <: B1 and A <: B2
        // -------------------
        // A <: (B1 & B2)
        (for {
          a <- newVariable
          b1 <- newVariable
          b2 <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            And(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b1))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b2))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(Variable(a), Value(ExprConstructor.IntersectionType, Seq(Variable(b1), Variable(b2)))),
            ),
          )
        )),

        // A <: B1 or A <: B2
        // ------------------
        // A <: (B1 | B2)
        (for {
          a <- newVariable
          b1 <- newVariable
          b2 <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            Or(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b1))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b2))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(Variable(a), Value(ExprConstructor.UnionType, Seq(Variable(b1), Variable(b2)))),
            ),
          )
        )),

        // A1 <: B or A2 <: B
        // ------------------
        // A1 & A2 <: B
        (for {
          a1 <- newVariable
          a2 <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            Or(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a1), Variable(b))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a2), Variable(b))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.IntersectionType, Seq(Variable(a1), Variable(a2))), Variable(b)),
            ),
          )
        )),

        // A1 <: B and A2 <: B
        // -------------------
        // (A1 | A2) <: B
        (for {
          a1 <- newVariable
          a2 <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            And(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a1), Variable(b))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a2), Variable(b))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.UnionType, Seq(Variable(a1), Variable(a2))), Variable(b)),
            ),
          )
        )),

        // A <: (A,)
        (for {
          a <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> PredicateFunction(
            ExprConstructor.SubtypeWitnessType,
            Seq(Variable(a), Value(ExprConstructor.LoadTuple, Seq(Variable(a)))),
          )
        )),

        // (A,) <: A
        (for {
          a <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> PredicateFunction(
            ExprConstructor.SubtypeWitnessType,
            Seq(Value(ExprConstructor.LoadTuple, Seq(Variable(a))), Variable(a)),
          )
        )),

        // A <: B and C <: D
        // --------------------
        // (B -> C) <: (A -> D)
        (for {
          a <- newVariable
          b <- newVariable
          c <- newVariable
          d <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            And(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(c), Variable(d))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(
                Value(ExprConstructor.FunctionType, Vector(Variable(b), Variable(c))),
                Value(ExprConstructor.FunctionType, Vector(Variable(a), Variable(d))),
              ),
            ),
          )
        )),

        // (B < A) == true
        // ------------------
        // TypeN A <: TypeN B
        (for {
          lt <- natLessThanFunction
          boolT <- boolType
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            PredicateFunction(
              ExprConstructor.EqualTo,
              Seq(
                Value(ExprConstructor.FunctionCall(lt), Seq(Variable(b), Variable(a))),
                Value(ExprConstructor.LoadConstantBool(true), Seq(wrapExprToExpr(boolT))),
              ),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(
                Value(ExprConstructor.TypeN, Seq(Variable(a))),
                Value(ExprConstructor.TypeN, Seq(Variable(b))),
              ),
            ),
          )
        )),

        // ------
        // A == A
        (for {
          a <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(WrapExpr.OfExpr(ArExpr(
            ExprConstructor.EqualTo,
            (
              WrapExpr.OfHole(a),
              WrapExpr.OfHole(a),
            ),
          )))) -> PredicateFunction(ExprConstructor.EqualTo, Seq(Variable(a), Variable(a)))
        )),
      ))

    protected override def assertions: ZIO[R, E, List[(Proof[ProofAtom], Predicate)]] = builtinAssertions

    private def newVariable: ZIO[R, E, ExprPrologSyntax.TVariable] =
      createHole.tap { hole =>
        createdHoles.update(_ + hole)
      }

    protected override def variableIsFromRules(variable: exprContext.THole): UIO[Boolean] =
      createdHoles.get.map(_.contains(variable))

    protected override def intrinsicPredicate
      (predicate: ExprConstructor, args: Seq[Expr], substitutions: Model, fuel: Int)
      : ZStream[R, Error, PrologResult.Yes] =
      (predicate, args) match {
        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.ClassType(classA), aArgs), Value(ExprConstructor.ClassType(classB), bArgs)),
            ) =>
          ZStream.unwrap(
            for {
              convAArgs <- ZIO.foreach(aArgs)(exprToWrapExprError)
              convBArgs <- ZIO.foreach(bArgs)(exprToWrapExprError)
            } yield resultIOToStream(
              isSubClass(classA, convAArgs, classB, convBArgs, fuel - 1).map(subClassResultToPrologResult(substitutions))
            )
          )

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.TraitType(traitA), aArgs), Value(ExprConstructor.TraitType(traitB), bArgs)),
            ) =>
          ZStream.unwrap(
            for {
              convAArgs <- ZIO.foreach(aArgs)(exprToWrapExprError)
              convBArgs <- ZIO.foreach(bArgs)(exprToWrapExprError)
            } yield resultIOToStream(
              isSubTrait(traitA, convAArgs, traitB, convBArgs, fuel - 1).map(subClassResultToPrologResult(substitutions))
            )
          )

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.ClassType(classA), aArgs), Value(ExprConstructor.TraitType(traitB), bArgs)),
            ) =>
          ZStream.unwrap(
            for {
              convAArgs <- ZIO.foreach(aArgs)(exprToWrapExprError)
              convBArgs <- ZIO.foreach(bArgs)(exprToWrapExprError)
            } yield resultIOToStream(
              classImplementsTrait(classA, convAArgs, traitB, convBArgs, fuel - 1).map(
                subClassResultToPrologResult(substitutions)
              )
            )
          )

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(a @ Value(ExprConstructor.LoadTuple, argsA), b @ Value(ExprConstructor.LoadTuple, argsB)),
            ) =>
          if argsA.size != argsB.size then {
            ZStream.unwrap(
              for {
                a2 <- exprToWrapExprError(a)
                b2 <- exprToWrapExprError(b)
                proof =
                  Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                    ExprConstructor.AssumeErasedValue,
                    EmptyTuple,
                  )))

              } yield ZStream.fail(Right(PrologResult.No(proof, substitutions)))
            )
          }
          else {
            argsA.zip(argsB)
              .map { case (argA, argB) => PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(argA, argB)) }
              .reduceOption(And.apply)
              .fold(ZStream.empty) { solve(_, substitutions, fuel) }
          }

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(
                a @ Value(ExprConstructor.TypeN, Seq(_)),
                b @ Value(ExprConstructor.OmegaTypeN(_) | ExprConstructor.AnyType, Seq()),
              ),
            ) =>
          ZStream.unwrap(
            for {
              a2 <- exprToWrapExprError(a)
              b2 <- exprToWrapExprError(b)
              proof =
                Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                  ExprConstructor.AssumeErasedValue,
                  EmptyTuple,
                )))

            } yield ZStream(PrologResult.Yes(proof, substitutions))
          )

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(a @ Value(ExprConstructor.OmegaTypeN(n1), Seq()), b @ Value(ExprConstructor.OmegaTypeN(n2), Seq())),
            ) =>
          if n1 <= n2 then {
            ZStream.unwrap(
              for {
                a2 <- exprToWrapExprError(a)
                b2 <- exprToWrapExprError(b)
                proof =
                  Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                    ExprConstructor.AssumeErasedValue,
                    EmptyTuple,
                  )))

              } yield ZStream(PrologResult.Yes(proof, substitutions))
            )
          }
          else {
            ZStream.unwrap(
              for {
                a2 <- exprToWrapExprError(a)
                b2 <- exprToWrapExprError(b)
                proof =
                  Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                    ExprConstructor.AssumeErasedValue,
                    EmptyTuple,
                  )))

              } yield ZStream.fail(Right(PrologResult.No(proof, substitutions)))
            )
          }

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(a @ Value(ExprConstructor.OmegaTypeN(_), Seq()), b @ Value(ExprConstructor.AnyType, Seq())),
            ) =>
          ZStream.unwrap(
            for {
              a2 <- exprToWrapExprError(a)
              b2 <- exprToWrapExprError(b)
              proof =
                Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                  ExprConstructor.AssumeErasedValue,
                  EmptyTuple,
                )))

            } yield ZStream(PrologResult.Yes(proof, substitutions))
          )

        case _ => ZStream.empty
      }

    protected override def mergeRelations(parentExprRelation: ExprRelation, subExprRelation: ExprRelation)
      : ExprRelation =
      (parentExprRelation, subExprRelation) match {
        case (ExprRelation.SyntacticEquality, _) | (_, ExprRelation.SyntacticEquality) => ExprRelation.SyntacticEquality
        case (ExprRelation.TypeEquality, _) | (_, ExprRelation.TypeEquality) => ExprRelation.TypeEquality
        // When checking for subtypes, use the expected variance
        case (ExprRelation.SubType, ExprRelation.SubType | ExprRelation.SuperType) => subExprRelation
        // When checking for supertypes, invert the variance
        case (ExprRelation.SuperType, ExprRelation.SuperType) => ExprRelation.SubType
        case (ExprRelation.SuperType, ExprRelation.SubType) => ExprRelation.SuperType
      }

    protected override def createConstraints(relation: ExprRelation, other: syntax.Expr): ExprConstraints[syntax.Expr] =
      relation match {
        case ExprRelation.SubType => ExprTypeBounds(subTypeBounds = Seq(other), superTypeBounds = Seq.empty)
        case ExprRelation.SuperType => ExprTypeBounds(superTypeBounds = Seq(other), subTypeBounds = Seq.empty)
        case ExprRelation.TypeEquality | ExprRelation.SyntacticEquality => ExprEqualConstraint(other)
      }

    protected override def createEqualityConstraint(other: syntax.Expr): ExprConstraints[syntax.Expr] =
      ExprEqualConstraint(other)

    protected override def swapRelation(relation: ExprRelation): ExprRelation =
      relation match {
        case ExprRelation.SubType => ExprRelation.SuperType
        case ExprRelation.SuperType => ExprRelation.SubType
        case ExprRelation.TypeEquality | ExprRelation.SyntacticEquality => relation
      }

    protected override def mergeConstraints(a: ExprConstraints[syntax.Expr], b: ExprConstraints[syntax.Expr])
      : Option[ExprConstraints[syntax.Expr]] =
      (a, b) match {
        case (ExprTypeBounds(aSuper, aSub), ExprTypeBounds(bSuper, bSub)) =>
          Some(ExprTypeBounds(aSuper ++ bSuper, aSub ++ bSub))

        case (ExprTypeBounds(_, _), fixed @ ExprEqualConstraint(_)) => Some(fixed)
        case (fixed @ ExprEqualConstraint(_), ExprTypeBounds(_, _)) => Some(fixed)

        case (ExprEqualConstraint(_), ExprEqualConstraint(_)) => None
      }

    protected override def predicateArgRelations(predicate: exprContext.ExprConstructor, arity: Int)
      : ZIO[R, E, Seq[ExprRelation]] = constructorArgRelations(predicate, arity)

    protected override def constructorArgRelations(constructor: exprContext.ExprConstructor, arity: Int)
      : ZIO[R, E, Seq[ExprRelation]] =
      constructor match {
        case ExprConstructor.ClassConstructorCall(ctor) => classConstructorRelations(ctor)
        case ExprConstructor.EnsureExecuted =>
          ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.FunctionCall(func) => functionRelations(func)
        case ExprConstructor.FunctionObjectCall =>
          ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.IfElse =>
          ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.BindVariable(_) =>
          ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.LoadConstantBool(_) => ZIO.succeed(Seq(ExprRelation.TypeEquality))
        case ExprConstructor.LoadConstantInt(_) => ZIO.succeed(Seq(ExprRelation.TypeEquality))
        case ExprConstructor.LoadConstantString(_) => ZIO.succeed(Seq(ExprRelation.TypeEquality))
        case ExprConstructor.LoadLambda(_) => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.LoadTuple => ZIO.succeed(Seq.fill(arity)(ExprRelation.SubType))
        case ExprConstructor.LoadTupleElement(_) => ZIO.succeed(Seq(ExprRelation.SubType))
        case ExprConstructor.LoadVariable(_) => ZIO.succeed(Seq.empty)
        case ExprConstructor.MethodCall(method) => methodRelations(method)
        case ExprConstructor.PatternMatch(_) => ZIO.succeed(Seq.fill(arity)(ExprRelation.SyntacticEquality))
        case ExprConstructor.RaiseException => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.Sequence => ZIO.succeed(Seq.fill(arity)(ExprRelation.SyntacticEquality))
        case ExprConstructor.StoreVariable(_) => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.TypeN => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.OmegaTypeN(_) | ExprConstructor.AnyType => ZIO.succeed(Seq.empty)
        case ExprConstructor.TraitType(arTrait) => traitRelations(arTrait)
        case ExprConstructor.ClassType(arClass) => classRelations(arClass)
        case ExprConstructor.FunctionType => ZIO.succeed(Seq(ExprRelation.SuperType, ExprRelation.SubType))
        case ExprConstructor.UnionType => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SubType))
        case ExprConstructor.IntersectionType => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SubType))
        case ExprConstructor.ExistentialType(_) => ZIO.succeed(Seq(ExprRelation.SubType))
        case ExprConstructor.ConjunctionType => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SubType))
        case ExprConstructor.DisjunctionType => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SubType))
        case ExprConstructor.NeverType => ZIO.succeed(Seq.empty)
        case ExprConstructor.SubtypeWitnessType => ZIO.succeed(Seq(ExprRelation.SuperType, ExprRelation.SubType))
        case ExprConstructor.EqualTo => ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.AssumeErasedValue => ZIO.succeed(Seq(ExprRelation.TypeEquality))
      }

    protected override def checkRelation
      (a: syntax.Expr, b: syntax.Expr, relation: ExprRelation, substitutions: Model, fuel: Int)
      : ZStream[R, Error, PrologResult.Yes] =
      relation match {
        case ExprRelation.SyntacticEquality =>
          solve(PredicateFunction(ExprConstructor.EqualTo, Seq(a, b)), substitutions, fuel)
        case ExprRelation.SubType =>
          solve(PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(a, b)), substitutions, fuel)
        case ExprRelation.SuperType =>
          solve(PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(b, a)), substitutions, fuel)
        case ExprRelation.TypeEquality =>
          solve(
            And(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(a, b)),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(b, a)),
            ),
            substitutions,
            fuel,
          )
      }

    protected override def otherForEquivalenceRelation(constraints: ExprConstraints[syntax.Expr]): Option[syntax.Expr] =
      constraints match {
        case ExprEqualConstraint(other) => Some(other)
        case ExprTypeBounds(_, _) => None
      }

    private def buildRelationProof(relation: ExprRelation, a: WrapExpr, b: WrapExpr): Proof[TCAtomicProof] =
      Proof.Atomic(TCAtomicProof.ExprProof(
        wrapExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)
      ))

    protected override def variableRelationProof(relation: ExprRelation, a: exprContext.THole, b: exprContext.THole)
      : ZIO[R, E, Proof[TCAtomicProof]] =
      ZIO.succeed(buildRelationProof(relation, WrapExpr.OfHole(a), WrapExpr.OfHole(b)))

    protected override def variableExprRelationProof(relation: ExprRelation, a: exprContext.THole, b: syntax.Expr)
      : ZIO[R, E, Proof[TCAtomicProof]] =
      exprToWrapExpr(b).map { b2 =>
        buildRelationProof(relation, WrapExpr.OfHole(a), b2)
      }

    protected override def valueRelationProof
      (relation: ExprRelation, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[TCAtomicProof]])
      : ZIO[R, E, Proof[TCAtomicProof]] =
      for {
        a2 <- exprToWrapExpr(a)
        b2 <- exprToWrapExpr(b)
      } yield buildRelationProof(relation, a2, b2)

    def exprToWrapExpr(expr: Expr): ZIO[R, E, WrapExpr] =
      expr match {
        case Value(ctor, args) =>
          ZIO.foreach(args)(exprToWrapExpr).flatMap { argExprs =>
            ctor.argsFromExprs(argExprs) match {
              case Some(ctorArgs) => ZIO.succeed(WrapExpr.OfExpr(ArExpr(ctor, ctorArgs)))
              case None => invalidExpr
            }
          }

        case Variable(variable) => ZIO.succeed(WrapExpr.OfHole(variable))
      }

    def exprToWrapExprError(expr: Expr): ZIO[R, Error, WrapExpr] = exprToWrapExpr(expr).mapError(Left.apply)

    def wrapExprToExpr(e: WrapExpr): Expr =
      e match {
        case WrapExpr.OfExpr(expr) =>
          Value(expr.constructor, expr.constructor.argsToExprs(expr.args).map(wrapExprToExpr))
        case WrapExpr.OfHole(variable) => Variable(variable)
      }

    def arExprToGoal(expr: WrapExpr, fuel: Int): ZIO[R, E, Predicate] =
      evaluator.normalizeTopLevelWrap(expr, fuel).flatMap {
        case WrapExpr.OfExpr(expr) =>
          expr.constructor match {
            case ExprConstructor.ConjunctionType =>
              val (a, b) = expr.args.asInstanceOf[ExprConstructor.ConjunctionTypeArgs]
              for {
                a2 <- arExprToGoal(a, fuel - 1)
                b2 <- arExprToGoal(b, fuel - 1)
              } yield And(a2, b2)

            case ExprConstructor.DisjunctionType =>
              val (a, b) = expr.args.asInstanceOf[ExprConstructor.DisjunctionTypeArgs]
              for {
                a2 <- arExprToGoal(a, fuel - 1)
                b2 <- arExprToGoal(b, fuel - 1)
              } yield Or(a2, b2)

            case ExprConstructor.FunctionType =>
              val (a, b) = expr.args.asInstanceOf[ExprConstructor.ConjunctionTypeArgs]
              for {
                a2 <- arExprToGoal(a, fuel - 1)
                b2 <- arExprToGoal(b, fuel - 1)
              } yield Implies(a2, b2)

            case ExprConstructor.NeverType => ZIO.succeed(PropFalse)

            case _ =>
              val args = expr.constructor.argsToExprs(expr.args).map(wrapExprToExpr)
              ZIO.succeed(PredicateFunction(expr.constructor, args))
          }

        case WrapExpr.OfHole(_) => invalidPredicateExpr
      }

    private def subClassResultToPrologResult(substitutions: Model)(result: SubClassResult): PrologResult =
      result match {
        case SubClassResult.SubClassProof(proof) =>
          PrologResult.Yes(Proof.Atomic(TCAtomicProof.ExprProof(proof)), substitutions)
        case SubClassResult.NotSubClassProof(proof) =>
          PrologResult.No(Proof.Atomic(TCAtomicProof.ExprProof(proof)), substitutions)

        case SubClassResult.Unknown =>
          PrologResult.Unknown
      }

  }

  final case class ResolvedImplicit(proof: Proof[TCAtomicProof], model: Map[THole, ExprConstraints[WrapExpr]])

  final def tryResolve(implicitType: WrapExpr, model: Map[THole, ExprConstraints[WrapExpr]], fuel: Int)
    : ZIO[R, E, Option[ResolvedImplicit]] =
    Ref.make(Set.empty[THole]).flatMap { createdHoles =>
      val context = new TCPrologContext(createdHoles)
      context.arExprToGoal(implicitType, fuel).flatMap { goal =>
        val proverModel = model.view.mapValues { _.map(context.wrapExprToExpr) }.toMap

        context.check(goal, proverModel, fuel).flatMap {
          case context.PrologResult.Yes(proof, model) =>
            ZIO.foreach(model) { case (hole, expr) =>
              expr.traverse(context.exprToWrapExpr).map { expr => (hole, expr) }
            }.map { model =>
              Some(ResolvedImplicit(proof, model))
            }
          case context.PrologResult.No(_, _) | _: context.PrologResult.Unknown.type => ZIO.none
        }
      }
    }

}