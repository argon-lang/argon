package dev.argon.compiler

import dev.argon.util.stream.PureEffect

trait CompilationRE[TCompRE[_, _, _], R] extends CompilationE[TCompRE[R, ?, ?]] {

  def fromPureEffect[A](pa: PureEffect[R, ErrorType, A]): TCompRE[R, ErrorType, A]

}

object CompilationRE {

  def apply[F[_, _, _], R](implicit compInstance: CompilationRE[F, R]): CompilationRE[F, R] = compInstance

}