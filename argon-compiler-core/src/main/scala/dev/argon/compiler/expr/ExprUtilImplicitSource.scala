package dev.argon.compiler.expr

trait ExprUtilImplicitSource extends ExprUtilBase {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  final case class ImplicitSource(implicits: List[ImplicitValue]) {
    def addVariable(variable: Variable): ImplicitSource =
      ImplicitSource(ImplicitValue.OfVariable(variable) :: implicits)
  }

  object ImplicitSource {
    def empty: ImplicitSource =
      ImplicitSource(Nil)
  }

  enum ImplicitValue {
    case OfVariable(variable: Variable)
  }


}
