package com.mi3software.argon.util

import scalaz._
import Scalaz._

class EitherFlattener[T] extends (Lambda[A => T \/ (T \/ A)] ~> (T \/ ?)) {

  override def apply[A](fa: T \/ (T \/ A)): T \/ A =
    fa match {
      case -\/(left) => -\/(left)
      case \/-(right) => right
    }

}

class EitherTFlattener[M[_] : Monad, T] extends (EitherT[EitherT[M, T, ?], T, ?] ~> EitherT[M, T, ?]) {

  override def apply[A](fa: EitherT[EitherT[M, T, ?], T, A]): EitherT[M, T, A] =
    EitherT(fa.run.run.map {
      case -\/(left) => -\/(left)
      case \/-(right) => right
    })

}
