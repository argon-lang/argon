package dev.argon.compiler

import cats.Monoid
import cats.data.NonEmptySeq
import dev.argon.expr.{Evaluator, HoleSubstitution, NullaryBuiltin, Unification}
import zio.interop.catz.core.given
import zio.stream.ZStream
import zio.{Ref, ZIO}

trait PatternExhaustiveCheck extends UsingContext {
  import context.{Env, Error, TRExprContext as exprContext, TRSignatureContext as signatureContext}
  import exprContext.{Builtin, Expr, Pattern}

  val evaluator: Evaluator[Env, Error] { val exprContext: context.TRExprContext.type }

  def makeHole(param: signatureContext.SignatureParameter): Comp[Expr]
  val model: exprContext.Model


  private enum Remnant derives CanEqual {
    case AnyValue(t: Expr)
    case BooleanValue(b: Boolean)
    case Tuple(items: Seq[Remnant])
    case EnumVariant(enumType: Expr.EnumType, variant: PatternExhaustiveCheck.this.EnumVariant, args: Seq[Remnant], fields: Map[RecordField, Remnant])

    def remnantType: Expr =
      this match {
        case AnyValue(t) => t
        case BooleanValue(_) => Expr.Builtin(exprContext.Builtin.Nullary(NullaryBuiltin.BoolType))
        case Tuple(items) => Expr.Tuple(items.map(_.remnantType))
        case EnumVariant(enumType, _, _, _) => enumType
      }
  }

  private final case class EnumVariantRemnant(variant: EnumVariant, args: Seq[Remnant], fields: Map[RecordField, Remnant])

  // Eliminates possible values from a remnant.
  private def eliminate(remnant: Remnant, pattern: Pattern): ZStream[Env, Error, Remnant] =
    expand(remnant).flatMap {
      case remnant @ Remnant.AnyValue(t) =>
        matchPattern[Nothing](remnant)(pattern)(PartialFunction.empty)(identity)

      case remnant @ Remnant.BooleanValue(b) =>
        matchPattern(remnant)(pattern) {
          case pattern @ Pattern.Bool(b2) if b == b2 => pattern
        } { _ => ZStream.empty }

      case remnant @ Remnant.Tuple(itemRemnants) =>
        matchPattern(remnant)(pattern) {
          case Pattern.Tuple(itemPatterns) if itemRemnants.size == itemPatterns.size => itemPatterns
        } { itemPatterns =>
          eliminateArgs(itemRemnants, itemPatterns)
            .map(Remnant.Tuple.apply)
        }


      case remnant @ Remnant.EnumVariant(enumType, variant, args, fields) =>
        matchPattern(remnant)(pattern) {
          case pattern @ Pattern.EnumVariant(_, variant2, _, _) if variant == variant2 => pattern
        } { enumPattern =>
          val orderedFieldPairs = fields.toSeq

          val argRemnants = args ++ orderedFieldPairs.iterator.map(_._2)
          val argPatterns = enumPattern.args ++ orderedFieldPairs.iterator.map { (field, fieldRemnant) =>
            enumPattern.fields.find(_.field == field)
              .fold(Pattern.Discard(fieldRemnant.remnantType))(_.pattern)
          }

          eliminateArgs(argRemnants, argPatterns)
            .map { argRemnants =>
              Remnant.EnumVariant(
                enumType,
                variant,
                argRemnants.take(args.size),
                orderedFieldPairs.view
                  .map { (field, _) => field }
                  .zip(argRemnants.view.drop(args.size))
                  .toMap,
              )
            }
        }
    }

  private def eliminateArgs(argRemnants: Seq[Remnant], argPatterns: Seq[Pattern]): ZStream[Env, Error, Seq[Remnant]] =
    ZStream.fromIterable(argRemnants.view.zip(argPatterns).zipWithIndex).flatMap {
      case ((argRemnant, argPattern), i) =>
        eliminate(argRemnant, argPattern).map { argRemnant =>
          argRemnants.updated(i, argRemnant)
        }
    }



  // Expands the top level of a remnant into its possible cases.
  // For example, if the type is an enum, it will expand into all of the variants.
  private def expand(remnant: Remnant): ZStream[Env, Error, Remnant] =
    remnant match {
      case Remnant.AnyValue(t) =>
        ZStream.unwrap(
          evaluator.normalizeToValue(t, context.Config.evaluatorFuel).map {
            case Expr.Builtin(Builtin.Nullary(NullaryBuiltin.BoolType)) =>
              ZStream(Remnant.BooleanValue(true), Remnant.BooleanValue(false))

            case Expr.Tuple(itemTypes) =>
              ZStream(Remnant.Tuple(itemTypes.map(Remnant.AnyValue.apply)))

            case t @ Expr.EnumType(e, args) =>
              ZStream.fromZIO(e.variants)
                .flatMap(ZStream.fromIterable(_))
                .flatMap { variant =>
                  ZStream.unwrap(
                    for
                      sig <- variant.signature
                      owner = exprContext.ParameterOwner.EnumVariant(variant)
                      (sig, holes) <- signatureContext.signatureFromDefault(sig).substituteHolesForArgs(owner)(makeHole)
                      model <- Ref.make(model)
                      isValid <- Unification.unify[context.Env, context.Error](exprContext)(model, evaluator, context.Config.evaluatorFuel)(t, sig.returnType)

                    yield
                      if isValid then
                        ZStream.unwrap(
                          for
                            args <- ZIO.foreach(sig.parameters) { param =>
                              fillHoles(model, param.paramType)
                            }

                            argRemnants = args.map(Remnant.AnyValue.apply)

                            fields <- variant.fields

                            fieldsRemnants = fields
                              .view
                              .map { field =>
                                val t = sig.substituteWithinExprForArgs(owner, args, DefaultToTRShifter[context.type](context).shiftExpr(field.fieldType))
                                field -> Remnant.AnyValue(t)
                              }
                              .toMap

                          yield ZStream(Remnant.EnumVariant(
                            t,
                            variant,
                            argRemnants,
                            fieldsRemnants,
                          ))
                        )
                      else
                        ZStream.empty

                  )
                }

            case _ =>
              ZStream(remnant)
          }
        )

      case _ => ZStream(remnant)
    }

  private def fillHoles(model: Ref[exprContext.Model], e: Expr): Comp[Expr] =
    model.get.map { model =>
      HoleSubstitution.substitute(exprContext)(model.mapping)(e)
    }


  private enum SimplifiedPattern[+A] derives CanEqual {
    case Wildcard
    case Unmatched
    case Patterns(patterns: NonEmptySeq[A])
  }

  private object SimplifiedPattern {
    given [A] => Monoid[SimplifiedPattern[A]]:
      override def empty: SimplifiedPattern[A] = SimplifiedPattern.Unmatched
      override def combine(x: SimplifiedPattern[A], y: SimplifiedPattern[A]): SimplifiedPattern[A] =
        (x, y) match {
          case (SimplifiedPattern.Wildcard, _) => SimplifiedPattern.Wildcard
          case (_, SimplifiedPattern.Wildcard) => SimplifiedPattern.Wildcard
          case (SimplifiedPattern.Unmatched, _) => y
          case (_, SimplifiedPattern.Unmatched) => x
          case (SimplifiedPattern.Patterns(xs), SimplifiedPattern.Patterns(ys)) => SimplifiedPattern.Patterns(xs ++: ys)
        }
    end given
  }

  private def matchPattern[A](remnant: Remnant)(pattern: Pattern)(patternSelector: PartialFunction[Pattern, A])(f: A => ZStream[Env, Error, Remnant]): ZStream[Env, Error, Remnant] =
    pattern match {
      case Pattern.Discard(_) => ZStream.empty
      case Pattern.Binding(_, pattern) => matchPattern(remnant)(pattern)(patternSelector)(f)
      case _ =>
        patternSelector
          .andThen(f)
          .applyOrElse(pattern, _ => ZStream(remnant))
    }


  private def crossProducts[A](s: Seq[ZStream[Env, Error, A]]): ZStream[Env, Error, Seq[A]] =
    s match {
      case Seq(h) => h.map(Seq(_))
      case h +: t =>
        for
          t2 <- crossProducts(t)
          h2 <- h
        yield h2 +: t2
      case _ => ZStream.succeed(Seq())
    }


  def check(t: Expr, patterns: Seq[Pattern]): Comp[Unit] =
    patterns
      .foldLeft(ZStream(Remnant.AnyValue(t)) : ZStream[Env, Error, Remnant]) { (acc, pattern) =>
        acc.flatMap(eliminate(_, pattern))
      }
      .take(1)
      .runCount
      .flatMap { count =>
        if count > 0 then ???
        else ZIO.unit
      }


}
