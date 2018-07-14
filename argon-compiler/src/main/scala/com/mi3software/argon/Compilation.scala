package com.mi3software.argon

import com.mi3software.argon.compiler.CompilationMessage

trait Compilation[F[_]] {

  def forErrors[A](value: A, errors: CompilationMessage*): F[A]

}

object Compilation {

  def apply[F[_] : Compilation]: Compilation[F] = implicitly[Compilation[F]]

}
