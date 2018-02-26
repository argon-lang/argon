package com.mi3software.argon.compiler
import com.mi3software.argon.compiler.ExpressionConverterConv.ConvState
import com.mi3software.argon.util.Compilation
import scalaz.{Monad, StateT}

trait ExpressionConverterConv extends ExpressionConverter {

  type Comp[T]
  protected implicit val compMonadInstance: Monad[Comp]
  protected implicit val compCompilationInstance: Compilation[Comp]

  override type Conv[T] = StateT[Comp, ConvState, T]

  override protected val monadInstance: Monad[Conv] = implicitly[Monad[Conv]]

  override protected val compilationInstance: Compilation[Conv] = new Compilation[Conv] {
    override def forErrors[A](value: A, errors: CompilationMessage*): Conv[A] =
      StateT(s => compCompilationInstance.forErrors((s, value), errors: _*))
  }



}

object ExpressionConverterConv {

  final case class ConvState(nextVariableId: Int)

}
