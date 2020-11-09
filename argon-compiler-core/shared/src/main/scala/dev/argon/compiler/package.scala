package dev.argon

import cats.data.NonEmptyList
import zio.{IO, Managed, ZIO, ZManaged}
import zio.stream.Stream

package object compiler {
  type CompilationError = DiagnosticError

  type Comp[+A] = IO[CompilationError, A]
  type RComp[-R, +A] = ZIO[R, CompilationError, A]

  type CompStream[+A] = Stream[CompilationError, A]

  type CompManaged[+A] = Managed[CompilationError, A]
  type RCompManaged[-R, +A] = ZManaged[R, CompilationError, A]
}
