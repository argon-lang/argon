package dev.argon.compiler.expr

import dev.argon.compiler.module.ModuleElementC
import zio.*

trait ExprUtilImplicitSource extends ExprUtilBase {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  trait ImplicitSource {
    def givenAssertions: Comp[List[ImplicitValue]]

    def addVariable(variable: Variable): ImplicitSource =
      new ImplicitSource:
        override def givenAssertions: Comp[List[ImplicitValue]] =
          for
            nextAssertions <- ImplicitSource.this.givenAssertions
          yield ImplicitValue.OfVariable(variable) :: nextAssertions
      end new
  }



  object ImplicitSource {
    def empty: ImplicitSource =
      new ImplicitSource:
        override def givenAssertions: Comp[List[ImplicitValue]] = ZIO.succeed(Nil)
      end new

    def fromImports(importsComp: Comp[Imports[context.type]], next: ImplicitSource): ImplicitSource =
      new ImplicitSource:
        override def givenAssertions: Comp[List[ImplicitValue]] =
          for
            nextAssertions <- next.givenAssertions
            imports <- importsComp
            functions = imports
              .valuesIterator
              .flatten
              .flatMap(moduleElementGivens)
              .toList
          yield functions ++ nextAssertions

        private def moduleElementGivens(element: ModuleElement[?]): Seq[ImplicitValue] =
          element match
            case ModuleElementC.FunctionElement(func) if func.isProof => Seq(ImplicitValue.OfFunction(func))
            case ModuleElementC.ExportedElement(_, _, inner) => moduleElementGivens(inner)
            case _ => Seq()
          end match

      end new
  }

  enum ImplicitValue {
    case OfVariable(variable: Variable)
    case OfFunction(function: ArFunc)
  }


}
