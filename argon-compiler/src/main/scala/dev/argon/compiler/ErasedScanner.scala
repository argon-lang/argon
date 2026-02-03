package dev.argon.compiler

import cats.*
import cats.data.OptionT
import cats.implicits.given
import dev.argon.expr.{ErasureMode, ExprScanner}
import dev.argon.util.*
import zio.interop.catz.core.given

object ErasedScanner {
  def apply
    (context: Context)
    (ec: context.ArgonExprContext { type Hole = Nothing })
    (sc: context.ArgonSignatureContextBase { val exprContext: ec.type })
    (expr: ec.Expr)
  : context.Comp[Boolean] =
    import context.Comp
    import ec.*

    val ctx: context.type = context

    type EraseScan[A] = OptionT[Comp, A]

    final class EraseScanner extends ExprScanner[EraseScan] {
      import StandardScanners.given

      override val exprContext: ec.type = ec

      private val concrete: EraseScan[Unit] = OptionT.some(())
      private val erased: EraseScan[Unit] = OptionT.none

      override def exprScanner: Scanner[Expr] = new Scanner[Expr] {
        override def scan(a: Expr): EraseScan[Unit] =
          a match {
            case Expr.BindVariable(v, value) if v.erasureMode == ErasureMode.Erased => concrete

            case Expr.Boxed(_) => concrete

            case Expr.FunctionCall(f, _) if f.erasureMode == ErasureMode.Erased => erased

            case Expr.FunctionCall(f, args) =>
              OptionT.liftF(f.signature).flatMap { sig =>
                scanArgs(sig, args)
              }

            case expr @ Expr.FunctionObjectCall(f, a) =>
              OptionT.liftF[Comp, Expr](
                new ExprType {
                  override val context: ctx.type = ctx
                  override val exprContext: ec.type = ec
                  override val sigContext: sc.type = sc

                  override protected def getHoleType(hole: Nothing): Expr = hole
                }.getExprType(f)
              )
                .flatMap { ft =>
                  val funcArgErased = ft match {
                    case Expr.FunctionType(v, _) => v.erasureMode == ErasureMode.Erased
                    case _ => false
                  }

                  if funcArgErased then concrete
                  else EraseScanner.super.exprScanner.scan(expr)
                }

            case Expr.RecordType(r, args) =>
              OptionT.liftF(r.signature).flatMap { sig =>
                scanArgs(sig, args)
              }

            case Expr.RecordFieldLoad(_, field, _) if field.erasureMode == ErasureMode.Erased => erased
            case Expr.RecordFieldStore(_, field, _, _) if field.erasureMode == ErasureMode.Erased => concrete

            case Expr.RecordLiteral(record, fields) =>
              scan(record) *> fields.traverse_ { field =>
                field.field.erasureMode match {
                  case ErasureMode.Erased => concrete
                  case _ => scan(field.value)
                } 
              }

            case Expr.Variable(v) =>
              if v.erasureMode == ErasureMode.Erased then erased
              else concrete

            case Expr.VariableStore(v, _) if v.erasureMode == ErasureMode.Erased => concrete

            case _ => EraseScanner.super.exprScanner.scan(a)
          }
      }

      private def scanArgs(sig: context.DefaultSignatureContext.FunctionSignature, args: Seq[Expr]): EraseScan[Unit] =
        sig.parameters.view.zip(args)
          .filter { (param, _) => param.erasureMode != ErasureMode.Erased }
          .map { (_, arg) => arg }
          .toSeq
          .traverse_(exprScanner.scan)


      override protected def holeScanner: Scanner[Hole] = summon
    }

    val scanner = EraseScanner()

    scanner.exprScanner.scan(expr).isEmpty
  end apply

}
