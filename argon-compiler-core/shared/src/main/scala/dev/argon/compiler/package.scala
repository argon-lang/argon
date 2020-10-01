package dev.argon

import cats.data.NonEmptyList
import zio.{IO, ZIO}

package object compiler {
  type CompError = CompilationError

  type Comp[+A] = IO[CompError, A]
  type RComp[-R, +A] = ZIO[R, CompError, A]
}
