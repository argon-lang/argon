package dev.argon

import cats.data.NonEmptyList
import zio.{IO, ZIO}

package object compiler {
  type CompilationError = DiagnosticError

  type Comp[+A] = IO[CompilationError, A]
  type RComp[-R, +A] = ZIO[R, CompilationError, A]
}
