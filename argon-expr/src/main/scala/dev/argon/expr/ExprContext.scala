package dev.argon.expr

import scala.reflect.TypeTest
import dev.argon.util.{*, given}

import java.util.Objects

trait ExprContext {

  type TClass
  type TTrait
  type TFunction
  type TMethod
  type TClassConstructor
  type TVariable
  type TLocalVariable <: TVariable

  type THole

  given classCanEqual: CanEqual[TClass, TClass]
  given traitCanEqual: CanEqual[TTrait, TTrait]
  given functionCanEqual: CanEqual[TFunction, TFunction]
  given methodCanEqual: CanEqual[TMethod, TMethod]
  given classConstructorCanEqual: CanEqual[TClassConstructor, TClassConstructor]
  given variableCanEqual: CanEqual[TVariable, TVariable]
  given localVariableCanEqual: CanEqual[TLocalVariable, TLocalVariable]

  given holeCanEqual: CanEqual[THole, THole]

  final class ArExpr[+A <: ExprConstructor](val constructor: A, val args: constructor.ConstructorArgs) {
    override def toString: String = s"$constructor($args)"

    def getArgs(ctor: constructor.type): ctor.ConstructorArgs = args

    override def hashCode(): Int =
      Objects.hash(constructor, args)

    override def equals(obj: Any): Boolean =
      obj match
        case other: ArExpr[_] => other.constructor.equals(constructor) && other.args.equals(args)
        case _ => false
      end match
  }

  enum WrapExpr {
    case OfExpr(expr: ArExpr[ExprConstructor])
    case OfHole(hole: THole)
  }

  def wrapExpr(constructor: ExprConstructor, args: constructor.ConstructorArgs): WrapExpr =
    WrapExpr.OfExpr(ArExpr(constructor, args))

  trait ArgumentCodec[Args] {
    def fromExprs(exprs: Seq[WrapExpr]): Option[(Args, Seq[WrapExpr])]
    def toExprs(args: Args): Seq[WrapExpr]
  }

  given ArgumentCodec[WrapExpr] =
    new ArgumentCodec[WrapExpr] {

      override def fromExprs(exprs: Seq[WrapExpr]): Option[(WrapExpr, Seq[WrapExpr])] =
        exprs match {
          case head +: tail => Some((head, tail))
          case _ => None
        }

      override def toExprs(args: WrapExpr): Seq[WrapExpr] = Seq(args)
    }

  given [Ctor <: ExprConstructor](using TypeTest[ExprConstructor, Ctor]): ArgumentCodec[ArExpr[Ctor]] =
    new ArgumentCodec[ArExpr[Ctor]] {

      override def fromExprs(exprs: Seq[WrapExpr]): Option[(ArExpr[Ctor], Seq[WrapExpr])] =
        exprs match {
          case WrapExpr.OfExpr(headExpr) +: tail =>
            headExpr.constructor match {
              case ctor: Ctor => Some((ArExpr(ctor, headExpr.args.asInstanceOf[ctor.ConstructorArgs]), tail))
              case _ => None
            }

          case _ => None
        }

      override def toExprs(args: ArExpr[Ctor]): Seq[WrapExpr] = Seq(WrapExpr.OfExpr(args))
    }

  given [H: ArgumentCodec, T <: Tuple : ArgumentCodec]: ArgumentCodec[H *: T] =
    new ArgumentCodec[H *: T] {

      override def fromExprs(exprs: Seq[WrapExpr]): Option[(H *: T, Seq[WrapExpr])] =
        summon[ArgumentCodec[H]].fromExprs(exprs).flatMap { case (h, exprs) =>
          summon[ArgumentCodec[T]].fromExprs(exprs).map { case (t, exprs) =>
            (h *: t, exprs)
          }
        }

      override def toExprs(args: H *: T): Seq[WrapExpr] = {
        val (h *: t) = args
        summon[ArgumentCodec[H]].toExprs(h) ++ summon[ArgumentCodec[T]].toExprs(t)
      }

    }

  given ArgumentCodec[EmptyTuple] =
    new ArgumentCodec[EmptyTuple] {
      override def fromExprs(exprs: Seq[WrapExpr]): Option[(EmptyTuple, Seq[WrapExpr])] = Some((EmptyTuple, exprs))

      override def toExprs(args: EmptyTuple): Seq[WrapExpr] = Seq.empty
    }

  given ArgumentCodec[Vector[WrapExpr]] =
    new ArgumentCodec[Vector[WrapExpr]] {

      override def fromExprs(exprs: Seq[WrapExpr]): Option[(Vector[WrapExpr], Seq[WrapExpr])] =
        Some((exprs.toVector, Seq.empty))

      override def toExprs(args: Vector[WrapExpr]): Seq[WrapExpr] = args
    }

  given [N <: Nat : NListFactory]: ArgumentCodec[NList[N, WrapExpr]] with

    override def fromExprs(exprs: Seq[WrapExpr]): Option[(NList[N, WrapExpr], Seq[WrapExpr])] =
      summon[NListFactory[N]].fromSeqPrefix(exprs)

    override def toExprs(args: NList[N, WrapExpr]): Seq[WrapExpr] = args.toSeq
  end given

  given ArgumentCodec[NonEmptyList[WrapExpr]] =
    new ArgumentCodec[NonEmptyList[WrapExpr]] {

      override def fromExprs(exprs: Seq[WrapExpr]): Option[(NonEmptyList[WrapExpr], Seq[WrapExpr])] =
        NonEmptyList.fromList(exprs.toList) match {
          case Some(nev) => Some((nev, Seq.empty))
          case None => None
        }

      override def toExprs(args: NonEmptyList[WrapExpr]): Seq[WrapExpr] = args.toList
    }

  sealed trait ExprConstructor derives CanEqual {
    type ConstructorArgs

    def argsFromExprs(exprs: Seq[WrapExpr]): Option[ConstructorArgs]
    def argsToExprs(args: ConstructorArgs): Seq[WrapExpr]
  }

  sealed abstract class ExprConstructorWithArgsBase[Args] extends ExprConstructor {
    type ConstructorArgs = Args

    def constructorArgsCodec: ArgumentCodec[Args]

    final def argsFromExprs(exprs: Seq[WrapExpr]): Option[ConstructorArgs] =
      constructorArgsCodec.fromExprs(exprs)
        .filter { case (_, remaining) => remaining.isEmpty }
        .map { case (args, _) => args }

    final def argsToExprs(args: ConstructorArgs): Seq[WrapExpr] = constructorArgsCodec.toExprs(args)
  }

  sealed abstract class ExprConstructorWithArgs[Args: ArgumentCodec] extends ExprConstructorWithArgsBase[Args] {
    override def constructorArgsCodec: ArgumentCodec[Args] = summon[ArgumentCodec[Args]]
  }

  object ExprConstructor {
    type TypeWithMethods = TraitType | ClassType

    final case class BindVariable(variable: TLocalVariable)
        extends ExprConstructorWithArgs[WrapExpr] with ExprConstructor derives CanEqual

    final case class ClassConstructorCall(classCtor: TClassConstructor)
        extends ExprConstructorWithArgs[ClassConstructorCallArgs] with ExprConstructor derives CanEqual

    case object EnsureExecuted extends ExprConstructorWithArgs[EnsureExecutedArgs] with ExprConstructor derives CanEqual

    final case class FunctionCall(function: TFunction) extends ExprConstructorWithArgs[ArgList] with ExprConstructor
        derives CanEqual

    case object FunctionObjectCall extends ExprConstructorWithArgs[FunctionObjectCallArgs] with ExprConstructor
        derives CanEqual

    case object IfElse extends ExprConstructorWithArgs[IfElseArgs] with ExprConstructor derives CanEqual

    final case class LoadConstantBool(b: Boolean) extends ExprConstructorWithArgs[EmptyTuple] with ExprConstructor
        derives CanEqual

    final case class LoadConstantInt(i: BigInt) extends ExprConstructorWithArgs[EmptyTuple] with ExprConstructor
        derives CanEqual

    final case class LoadConstantString(s: String) extends ExprConstructorWithArgs[EmptyTuple] with ExprConstructor
        derives CanEqual

    final case class LoadLambda(argVariable: TLocalVariable)
        extends ExprConstructorWithArgs[WrapExpr] with ExprConstructor derives CanEqual

    case object LoadTuple extends ExprConstructorWithArgs[ArgList] with ExprConstructor derives CanEqual

    final case class LoadTupleElement(index: Int) extends ExprConstructorWithArgs[WrapExpr] with ExprConstructor
        derives CanEqual

    final case class LoadVariable(variable: TVariable) extends ExprConstructorWithArgs[EmptyTuple] with ExprConstructor
        derives CanEqual

    final case class MethodCall(method: TMethod) extends ExprConstructorWithArgs[MethodCallArgs] with ExprConstructor
        derives CanEqual

    final case class PatternMatch[N <: Nat](patterns: NList[N, PatternExpr])
        extends ExprConstructorWithArgsBase[PatternMatchArgs[N]] with ExprConstructor {

      override def constructorArgsCodec: ArgumentCodec[(WrapExpr, NList[N, WrapExpr])] =
        given factory: NListFactory[N] = patterns.makeFactory
        summon[ArgumentCodec[(WrapExpr, NList[N, WrapExpr])]]
      end constructorArgsCodec

    }

    case object RaiseException extends ExprConstructorWithArgs[WrapExpr] with ExprConstructor derives CanEqual

    case object Sequence extends ExprConstructorWithArgs[NonEmptyArgList] with ExprConstructor derives CanEqual

    final case class StoreVariable(variable: TVariable) extends ExprConstructorWithArgs[WrapExpr] with ExprConstructor
        derives CanEqual

    case object TypeN extends ExprConstructorWithArgs[WrapExpr] with ExprConstructor derives CanEqual

    final case class OmegaTypeN(level: BigInt) extends ExprConstructorWithArgs[EmptyTuple] with ExprConstructor
        derives CanEqual

    case object AnyType extends ExprConstructorWithArgs[EmptyTuple] with ExprConstructor derives CanEqual

    final case class TraitType(arTrait: TTrait) extends ExprConstructorWithArgs[ArgList] with ExprConstructor
        derives CanEqual

    final case class ClassType(arClass: TClass) extends ExprConstructorWithArgs[ArgList] with ExprConstructor
        derives CanEqual

    case object FunctionType extends ExprConstructorWithArgs[FunctionTypeArgs] with ExprConstructor derives CanEqual
    case object UnionType extends ExprConstructorWithArgs[UnionTypeArgs] with ExprConstructor derives CanEqual

    case object IntersectionType extends ExprConstructorWithArgs[IntersectionTypeArgs] with ExprConstructor
        derives CanEqual

    final case class ExistentialType(variable: TLocalVariable)
        extends ExprConstructorWithArgs[WrapExpr] with ExprConstructor derives CanEqual

    case object ConjunctionType extends ExprConstructorWithArgs[ConjunctionTypeArgs] with ExprConstructor
        derives CanEqual

    case object DisjunctionType extends ExprConstructorWithArgs[DisjunctionTypeArgs] with ExprConstructor
        derives CanEqual

    case object NeverType extends ExprConstructorWithArgs[EmptyTuple] with ExprConstructor derives CanEqual

    case object SubtypeWitnessType extends ExprConstructorWithArgs[SubtypeWitnessTypeArgs] with ExprConstructor
        derives CanEqual

    case object EqualTo extends ExprConstructorWithArgs[EqualToArgs] with ExprConstructor derives CanEqual
    case object AssumeErasedValue extends ExprConstructorWithArgs[EmptyTuple] with ExprConstructor derives CanEqual

    type ClassConstructorCallArgs = (ArExpr[ClassType], Vector[WrapExpr])
    type EnsureExecutedArgs = (WrapExpr, WrapExpr)
    type FunctionObjectCallArgs = (WrapExpr, WrapExpr)
    type IfElseArgs = (WrapExpr, WrapExpr, WrapExpr)
    type MethodCallArgs = (WrapExpr, Vector[WrapExpr])
    type PatternMatchArgs[N <: Nat] = (WrapExpr, NList[N, WrapExpr])
    type FunctionTypeArgs = (WrapExpr, WrapExpr)
    type UnionTypeArgs = (WrapExpr, WrapExpr)
    type IntersectionTypeArgs = (WrapExpr, WrapExpr)
    type ConjunctionTypeArgs = (WrapExpr, WrapExpr)
    type DisjunctionTypeArgs = (WrapExpr, WrapExpr)
    type SubtypeWitnessTypeArgs = (WrapExpr, WrapExpr)
    type SubtypeWitnessArgs = (WrapExpr, WrapExpr)
    type NotSubtypeWitnessArgs = (WrapExpr, WrapExpr)
    type EqualToArgs = (WrapExpr, WrapExpr)
    type ArgList = Vector[WrapExpr]
    type NonEmptyArgList = NonEmptyList[WrapExpr]
  }

  sealed trait PatternExpr derives CanEqual

  object PatternExpr {
    final case class Binding(variable: TLocalVariable) extends PatternExpr derives CanEqual
    final case class CastBinding(variable: TLocalVariable) extends PatternExpr derives CanEqual
  }

}
