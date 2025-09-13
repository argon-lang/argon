package dev.argon.compiler

import cats.*
import cats.data.OptionT
import cats.implicits.given
import dev.argon.ast.IdentifierExpr
import dev.argon.expr.{BinaryBuiltin, ErasureMode, ExprContext, NullaryBuiltin, UnaryBuiltin}
import dev.argon.util.{*, given}
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

    trait PureScanner extends TreeScanner[EraseScan] {
      def exprScanner: Scanner[Expr]
    }

    val scanner = new PureScanner {
      import StandardScanners.given

      private val concrete: EraseScan[Unit] = OptionT.some(())
      private val erased: EraseScan[Unit] = OptionT.none
      private def fromBool(b: Boolean): EraseScan[Unit] = if b then erased else concrete

      override given exprScanner: Scanner[Expr]:
        override def scan(a: Expr): EraseScan[Unit] =
          a match {
            case Expr.BindVariable(v, value) if v.isErased => concrete

            case Expr.Boxed(_) => concrete

            case Expr.FunctionCall(f, _) if f.isErased => erased

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
                    case Expr.FunctionType(v, _) => v.isErased
                    case _ => true
                  }

                  if funcArgErased then concrete
                  else exprAutoScanner.scan(expr)
                }

            case Expr.RecordType(r, args) =>
              OptionT.liftF(r.signature).flatMap { sig =>
                scanArgs(sig, args)
              }

            case Expr.RecordFieldLoad(_, field, _) if field.isErased => erased
            case Expr.RecordFieldStore(_, field, _, _) if field.isErased => concrete

            case Expr.RecordLiteral(record, fields) =>
              scan(record) *> fields.traverse_ { field =>
                if field.field.isErased then concrete
                else scan(field.value)
              }

            case Expr.Variable(v) => fromBool(v.isErased)

            case Expr.VariableStore(v, _) if v.isErased => concrete

            case _ => exprAutoScanner.scan(a)
          }
      end exprScanner

      private def scanArgs(sig: context.DefaultSignatureContext.FunctionSignature, args: Seq[Expr]): EraseScan[Unit] =
        sig.parameters.view.zip(args)
          .filter { (param, _) => !param.isErased }
          .map { (_, arg) => arg }
          .toSeq
          .traverse_(exprScanner.scan)


      private val exprAutoScanner: Scanner[Expr] = autoScanner

      private given Scanner[Pattern] = autoScanner      
      private given Scanner[Builtin] = autoScanner
      private given Scanner[LocalVar] = autoScanner
      private given Scanner[Var] = autoScanner
      given Scanner[Expr.RecordType] = autoScanner
      given Scanner[Expr.EnumType] = autoScanner
      given Scanner[RecordFieldLiteral] = autoScanner
      private given Scanner[RecordFieldPattern] = autoScanner
      private given Scanner[MatchCase] = autoScanner

      private given Scanner[ParameterOwner] = IgnoreScanner[ParameterOwner]
      private given Scanner[Function] = IgnoreScanner[Function]
      private given Scanner[Record] = IgnoreScanner[Record]
      private given Scanner[RecordField] = IgnoreScanner[RecordField]
      private given Scanner[Enum] = IgnoreScanner[Enum]
      private given Scanner[EnumVariant] = IgnoreScanner[EnumVariant]
      private given Scanner[NullaryBuiltin] = IgnoreScanner[NullaryBuiltin]
      private given Scanner[UnaryBuiltin] = IgnoreScanner[UnaryBuiltin]
      private given Scanner[BinaryBuiltin] = IgnoreScanner[BinaryBuiltin]


      private given Scanner[UniqueIdentifier] = IgnoreScanner[UniqueIdentifier]
      private given Scanner[IdentifierExpr] = IgnoreScanner[IdentifierExpr]
      private given Scanner[Boolean] = IgnoreScanner[Boolean]
      private given Scanner[BigInt] = IgnoreScanner[BigInt]
      private given Scanner[Int] = IgnoreScanner[Int]
      private given Scanner[String] = IgnoreScanner[String]
    }

    scanner.exprScanner.scan(expr).isEmpty
  end apply

}
