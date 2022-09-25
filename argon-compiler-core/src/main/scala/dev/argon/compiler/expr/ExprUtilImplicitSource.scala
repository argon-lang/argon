package dev.argon.compiler.expr

import dev.argon.compiler.module.ModuleElementC
import dev.argon.compiler.tube.TubeName
import zio.*

trait ExprUtilImplicitSource extends ExprUtilBase {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  trait ImplicitSource {
    def givenAssertions: Comp[List[ImplicitValue]]

    def excludeTube(tube: TubeName): ImplicitSource

    def addVariable(variable: Variable): ImplicitSource =
      new ImplicitSource:
        override def givenAssertions: Comp[List[ImplicitValue]] =
          for
            nextAssertions <- ImplicitSource.this.givenAssertions
          yield ImplicitValue.OfVariable(variable) :: nextAssertions

        override def excludeTube(tube: TubeName): ImplicitSource =
          ImplicitSource.this.excludeTube(tube)
            .addVariable(variable)
      end new
  }



  object ImplicitSource {
    def empty: ImplicitSource =
      new ImplicitSource:
        override def givenAssertions: Comp[List[ImplicitValue]] = ZIO.succeed(Nil)
        override def excludeTube(tube: TubeName): ImplicitSource = this
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

        override def excludeTube(tube: TubeName): ImplicitSource =
          fromImports(
            importsComp.map { imports =>
              imports
                .view
                .mapValues { elements =>
                  elements.filterNot { element =>
                    element.module.tube.tubeName == tube
                  }
                }
                .filter { (_, elements) => elements.nonEmpty }
                .toMap
            },
            next
          )
      end new
  }

  enum ImplicitValue {
    case OfVariable(variable: Variable)
    case OfFunction(function: ArFunc)
  }


}
