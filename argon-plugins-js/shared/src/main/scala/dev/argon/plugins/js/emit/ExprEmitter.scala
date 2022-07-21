package dev.argon.plugins.js.emit

import dev.argon.util.*
import dev.argon.plugins.js.*
import dev.argon.compiler.*
import zio.*
import zio.stm.*

private[emit] trait ExprEmitter extends EmitModuleCommon {

  val nextLocalId: TRef[Int]
  val localNames: TSet[String]

  private def getNewLocalName: UIO[String] =
    (
      for
        id <- nextLocalId.getAndUpdate(_ + 1)
        name = s"arlocal$id"
        _ <- localNames.put(name)
      yield name
    ).commit

  import context.ExprContext.{ArExpr, ExprConstructor, WrapExpr}

  def classExport(arClass: ArClass): Comp[estree.ExportNamedDeclaration] = ???

  def traitExport(arTrait: ArTrait): Comp[estree.ExportNamedDeclaration] = ???

  def functionExport(func: ArFunc): Comp[estree.ExportNamedDeclaration] = ???

  private def emitWrapExprAsStmt(expr: WrapExpr): Comp[Seq[estree.Statement]] =
    expr match
      case WrapExpr.OfExpr(expr) => emitExprAsStmt(expr)
      case WrapExpr.OfHole(hole) => hole
    end match

  private def emitExprAsStmt(expr: ArExpr[ExprConstructor]): Comp[Seq[estree.Statement]] =
    expr.constructor match
      case _ =>
        for
          jsExpr <- emitExpr(expr)
        yield Seq(estree.ExpressionStatement(
          expression = jsExpr
        ))
    end match

  private def emitWrapExpr(expr: WrapExpr): Comp[estree.Expression] =
    expr match
      case WrapExpr.OfExpr(expr) => emitExpr(expr)
      case WrapExpr.OfHole(hole) => hole
    end match

  private def emitExpr(expr: ArExpr[ExprConstructor]): Comp[estree.Expression] =
    expr.constructor match
      case ctor: (expr.constructor.type & ExprConstructor.BindVariable) =>
        val value: WrapExpr = expr.getArgs(ctor)
        for
          valueExpr <- emitWrapExpr(value)
          varName <- getNewLocalName
        yield estree.AssignmentExpression(
          operator = "=",
          left = estree.Identifier(name = varName),
          right = valueExpr,
        )

      case ctor: (expr.constructor.type & ExprConstructor.FunctionCall) =>
        ???

      case ctor: (expr.constructor.type & ExprConstructor.LoadConstantBool) =>
        ZIO.succeed(estree.Literal(
          value = Nullable(ctor.b),
        ))

      case ctor: (expr.constructor.type & ExprConstructor.LoadConstantInt) =>
        ZIO.succeed(estree.Literal(
          value = Nullable(ctor.i),
          bigint = Some(ctor.i.toString)
        ))

      case ctor: (expr.constructor.type & ExprConstructor.LoadConstantString) =>
        ZIO.succeed(estree.Literal(
          value = Nullable(ctor.s),
        ))

      case _ => ???
    end match

}


