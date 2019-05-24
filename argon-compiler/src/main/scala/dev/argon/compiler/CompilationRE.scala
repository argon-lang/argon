package dev.argon.compiler

trait CompilationRE[TCompRE[_, _, _]] extends CompilationE[TCompRE[Any, ?, ?]]

object CompilationRE {

  def apply[F[_, _, _] : CompilationRE]: CompilationRE[F] = implicitly[CompilationRE[F]]

}