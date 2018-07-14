package com.mi3software.argon.compiler

import com.mi3software.argon.compiler.ExpressionConverterConv.ConvState
import com.mi3software.argon.Compilation

import scalaz._

trait ExpressionConverterConv extends ExpressionConverter {

  type Comp[T]
  protected implicit val compMonadInstance: Monad[Comp]
  protected implicit val compCompilationInstance: Compilation[Comp]

  override type Conv[T] = StateT[Comp, ConvState, T]

  override protected implicit val monadInstance: Monad[Conv] = ExpressionConverterConv.monadInstance

  override protected implicit val compilationInstance: Compilation[Conv] = new Compilation[Conv] {
    override def forErrors[A](value: A, errors: CompilationMessage*): Conv[A] =
      StateT(s => compCompilationInstance.forErrors((s, value), errors: _*))
  }

  override protected def nextVariableId: StateT[Comp, ConvState, Int] =
    (
      for {
        convState <- State.get[ConvState]
        _ <- State.put(convState.copy(nextVariableId = convState.nextVariableId + 1))
      } yield convState.nextVariableId
    ).lift[Comp]
}

object ExpressionConverterConv {

  final case class ConvState(nextVariableId: Int)

  object ConvState {
    val Default = ConvState(0)
  }

  private def monadInstance[F[_] : Monad]: Monad[StateT[F, ConvState, ?]] = implicitly[Monad[StateT[F, ConvState, ?]]]

}
