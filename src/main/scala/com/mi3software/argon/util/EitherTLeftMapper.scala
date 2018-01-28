package com.mi3software.argon.util

import scalaz._

final class EitherTLeftMapper[M[_] : Functor, T, U](f: T => U) extends (EitherT[M, T, ?] ~> EitherT[M, U, ?]) {

  override def apply[A](fa: EitherT[M, T, A]): EitherT[M, U, A] =
    fa.leftMap(f)

}
