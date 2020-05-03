package dev.argon

import cats.data.NonEmptyList
import zio.{IO, ZIO}

package object compiler {
  type ErrorList = NonEmptyList[CompilationError]

  type Comp[+A] = IO[ErrorList, A]
  type RComp[-R, +A] = ZIO[R, ErrorList, A]
}
