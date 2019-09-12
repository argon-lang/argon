package dev.argon.compiler


trait CompilationRE[TCompRE[_, _, _], R] extends CompilationE[TCompRE[R, ?, ?]] {

}

object CompilationRE {

  def apply[F[_, _, _], R](implicit compInstance: CompilationRE[F, R]): CompilationRE[F, R] = compInstance

}