package com.mi3software.argon.compiler
import com.mi3software.argon.util.Compilation
import scalaz.Monad

trait ExpressionConverterCombined[TContext <: Context]
  extends ExpressionConverterContext[TContext]
  with ExpressionConverterConv
{

  override type Comp[T] = context.Comp[T]
  override protected implicit val compMonadInstance: Monad[Comp] = context.compMonadInstance
  override protected implicit val compCompilationInstance: Compilation[Comp] = context.compCompilationInstance

  final class ExprTypes extends ArExprTypes {
    override val typeSystem: HoleTypeSystem[context.type] = new HoleTypeSystem[context.type]()
    override type TExpr = ArExpr[ExprTypes]
    override type TFunction = ArFunc[context.type]
    override type TMethod = ArMethod[context.type]
    override type TClassConstructor = ClassConstructor[context.type]
  }

  override val exprTypes: ExprTypes = new ExprTypes

  override type TScopeTypes = ScopeTypes with ({
    type TTrait = ArTrait[context.type]
    type TClass = ArClass[context.type]
    type TDataConstructor = DataConstructor[context.type]
    type TFunc = ArFunc[context.type]
    type TVariable = Variable[TS, VariableLikeDescriptor]
  })

}
