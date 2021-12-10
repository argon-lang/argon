package dev.argon.compiler

import dev.argon.compiler.backend.*
import zio.*

trait Context {
  val backend: BackendBase
  type Comp[+A] = ZIO[Any, Nothing, A]
}

object Context {
  type WithBackend[TBackend <: BackendBase] = Context { val backend: TBackend }
}

