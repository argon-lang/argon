package dev.argon.compiler

import zio._

trait Context {
  type Comp[+A] = ZIO[Any, Nothing, A]
}
