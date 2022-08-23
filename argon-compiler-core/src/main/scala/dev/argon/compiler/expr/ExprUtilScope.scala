package dev.argon.compiler.expr

import dev.argon.compiler.module.ModuleElementC
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{*, given}

trait ExprUtilScope extends ExprUtilBase {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  enum LookupResult[+TElement] {
    case Success(overloads: Seq[TElement], nextPriority: LookupResult[TElement])
    case Suspended(suspendedResult: Comp[LookupResult[TElement]])
    case NotFound()
  }

  final case class ParameterVariableElement(paramVar: exprContext.ParameterVariable, index: Int, elementType: WrapExpr)

  type ScopeElement = ModuleElementC[context.type, ?] | Variable | ParameterVariableElement

  trait Scope {
    def lookup(id: IdentifierExpr): LookupResult[ScopeElement]

    final def addVariable(variable: Variable): Scope =
      variable.name match
        case Some(id) => Scope.WithValue(id, variable, this)
        case None => this
      end match

    final def addVariables(variables: Seq[Variable]): Scope =
      variables.foldLeft(this)(_.addVariable(_))

    def addParameterVariableElements(map: Map[IdentifierExpr, ParameterVariableElement]): Scope =
      Scope.WithMap(map, this)


  }

  object Scope {

    private final class WithValue(id: IdentifierExpr, value: ScopeElement, next: Scope) extends Scope {
      override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
        if this.id == id then
          LookupResult.Success(Seq(value), LookupResult.NotFound())
        else
          next.lookup(id)
    }

    private final class WithMap(map: Map[IdentifierExpr, ScopeElement], next: Scope) extends Scope {
      override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
        map.get(id) match
          case Some(value) =>
            LookupResult.Success(Seq(value), LookupResult.NotFound())
          case None =>
            next.lookup(id)
        end match
    }

    def empty: Scope = new Scope {
      override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] = LookupResult.NotFound()
    }

    def fromImports(importsComp: Comp[Imports[context.type]], next: Scope): Scope =
      new Scope {

        override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
          LookupResult.Suspended(
            importsComp.map { imports =>
              imports.get(id) match {
                case None | Some(Seq()) => next.lookup(id)
                case Some(elements) => LookupResult.Success(elements, LookupResult.NotFound())
              }
            }
          )

      }

  }
}
