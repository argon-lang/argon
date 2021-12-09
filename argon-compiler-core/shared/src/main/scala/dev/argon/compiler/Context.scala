package dev.argon.compiler

import zio.*

trait Context {
  type Comp[+A] = ZIO[Any, Nothing, A]
}
