package com.mi3software.argon.util

import scalaz._
import Scalaz._

object MonadHelpers {

  def findFirst[A, B, F[_] : Monad, S[_]: Foldable](items: S[A])(f: A => F[Option[B]]): F[Option[B]] =
    items.foldLeftM(None : Option[B]) {
      case (result @ Some(_), _) => (result : Option[B]).point[F]
      case (None, item) => f(item)
    }

}
