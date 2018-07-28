package com.mi3software.argon.compiler

import com.mi3software.argon.compiler.ExpressionConverterConv.ConvState
import com.mi3software.argon.Compilation

import scalaz._

trait ExpressionConverterConv extends ExpressionConverter {

  type Comp[T]
  protected implicit val compCompilationInstance: Compilation[Comp]

  override type Conv[T] = StateT[Comp, ConvState, T]
  private lazy val convMonadTrans = StateT.StateMonadTrans[ConvState]
  private lazy val convMonadInstance = convMonadTrans(compCompilationInstance)

  override protected implicit val compilationInstance: Compilation[Conv] = new Compilation[Conv] {

    override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): Conv[A] =
      convMonadTrans.liftM(compCompilationInstance.diagnostic(value, messages))

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): Conv[A] =
      convMonadTrans.liftM(compCompilationInstance.forErrors(errors, messages))

    override def bind[A, B](fa: Conv[A])(f: A => Conv[B]): Conv[B] = convMonadInstance.bind(fa)(f)

    override def point[A](a: => A): StateT[Comp, ConvState, A] = convMonadInstance.point(a)

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


}
