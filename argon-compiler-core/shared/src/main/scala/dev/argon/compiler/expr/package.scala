package dev.argon.compiler

import cats.{Monad, Traverse}

package object expr {
  type ArExprWrap[TContext, Wrap[+_]] = Wrap[ArExpr[TContext, Wrap]]
  type WrapperInstance[F[_]] = Traverse[F] with Monad[F]
}
