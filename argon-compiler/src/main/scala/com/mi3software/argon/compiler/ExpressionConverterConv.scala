package com.mi3software.argon.compiler

import com.mi3software.argon.compiler.ExpressionConverterConv.ConvState
import scalaz._

trait ExpressionConverterConv extends ExpressionConverter {

  override type Conv[T] = StateT[context.Comp, ConvState, T]
  private lazy val convMonadTrans = StateT.StateMonadTrans[ConvState]
  private lazy val convMonadInstance = convMonadTrans(context.compCompilationInstance)

  override implicit val compilationInstance: Compilation[Conv] = new Compilation[Conv] {

    override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): Conv[A] =
      convMonadTrans.liftM(context.compCompilationInstance.diagnostic(value, messages))(context.compCompilationInstance)

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): Conv[A] =
      convMonadTrans.liftM(context.compCompilationInstance.forErrors(errors, messages))(context.compCompilationInstance)

    override def bind[A, B](fa: Conv[A])(f: A => Conv[B]): Conv[B] = convMonadInstance.bind(fa)(f)

    override def point[A](a: => A): StateT[context.Comp, ConvState, A] = convMonadInstance.point(a)

  }


  override lazy val compToConv: context.Comp ~> StateT[context.Comp, ConvState, ?] = new (context.Comp ~> StateT[context.Comp, ConvState, ?]) {
    override def apply[A](fa: context.Comp[A]): StateT[context.Comp, ConvState, A] =
      StateT.liftM(fa)(context.compCompilationInstance)
  }

  override val runConv: StateT[context.Comp, ConvState, ?] ~> context.Comp = new (StateT[context.Comp, ConvState, ?] ~> context.Comp) {
    override def apply[A](fa: StateT[context.Comp, ConvState, A]): context.Comp[A] =
      fa.eval(ConvState.Default)(context.compCompilationInstance)
  }

  override protected def nextVariableId: StateT[context.Comp, ConvState, Int] =
    (
      for {
        convState <- State.get[ConvState]
        _ <- State.put(convState.copy(nextVariableId = convState.nextVariableId + 1))
      } yield convState.nextVariableId
    ).lift[context.Comp](implicitly, context.compCompilationInstance)

  final class ExprTypes extends ArExprTypes {
    override val typeSystem: HoleTypeSystem[context.type] = new HoleTypeSystem[context.type]()
    override type TExpr = ArExpr[ExprTypes]
    override type TFunction = ArFunc[context.type]
    override type TMethod = ArMethod[context.type]
    override type TClassConstructor = ClassConstructor[context.type]
  }

  override val exprTypes: ExprTypes = new ExprTypes

  override type TScopeTypes = ConvScopeTypes[context.type, TS]

  override val scopeTypeConverter: ScopeTypeConverter[context.ContextScopeTypes, TScopeTypes] =
    ArgonToHoleScopeTypeConverter(context)

  override lazy val contextTypeSystemConverter: TypeSystemConverter[context.typeSystem.type, TS] =
    ArgonToHoleTypeSystemConverter(context)(exprTypes.typeSystem)

  override lazy val reverseTypeSystemConverter: TypeSystemConverter[TS, context.typeSystem.type] =
    HoleToArgonTypeSystemConverter(context)(exprTypes.typeSystem)

  override lazy val typeComparer: TypeComparerUnerased[TS] = ??? : TypeComparerUnerased[TS]


}

object ExpressionConverterConv {

  final case class ConvState(nextVariableId: Int)

  object ConvState {
    val Default = ConvState(0)
  }


}
