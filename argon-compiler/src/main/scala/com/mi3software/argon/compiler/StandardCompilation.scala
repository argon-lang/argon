package com.mi3software.argon.compiler


import com.mi3software.argon.Compilation
import com.mi3software.argon.util.CovariantMonad
import scalaz._

import scala.collection.immutable._

object StandardCompilation {

  private type StandardCompState[A] = State[Set[CompilationMessageNonFatal], NonEmptyList[CompilationError] \/ A]
  type StandardCompilationType[+A] = CovariantMonad[EitherT[State[Set[CompilationMessageNonFatal], ?], NonEmptyList[CompilationError], ?], A]

  implicit val standardCompilationInstance: Compilation[StandardCompilationType] = new Compilation[StandardCompilationType] {

    private def eitherTInstance = EitherT.eitherTMonad[State[Set[CompilationMessageNonFatal], ?], NonEmptyList[CompilationError]](StateT.stateMonad[Set[CompilationMessageNonFatal]])

    private def monadInstance: Monad[StandardCompilationType] =
      CovariantMonad.monadInstance[EitherT[State[Set[CompilationMessageNonFatal], ?], NonEmptyList[CompilationError], ?]](eitherTInstance)

    private def fromState[A](state: StandardCompState[A]): StandardCompilationType[A] =
      CovariantMonad[EitherT[State[Set[CompilationMessageNonFatal], ?], NonEmptyList[CompilationError], ?], A](
        EitherT[State[Set[CompilationMessageNonFatal], ?], NonEmptyList[CompilationError], A](state)
      )(eitherTInstance)

    private def fromStateF[A](f: Set[CompilationMessageNonFatal] => (Set[CompilationMessageNonFatal], NonEmptyList[CompilationError] \/ A)): StandardCompilationType[A] =
      fromState(State(f))

    override def point[A](a: => A): StandardCompilationType[A] =
      monadInstance.point(a)

    override def ap[A, B](fa: => StandardCompilationType[A])(f: => StandardCompilationType[A => B]): StandardCompilationType[B] =
      fromStateF(prevMsgs => {
        val (msgsA, resA) = fa.run.run(prevMsgs) : (Set[CompilationMessageNonFatal], NonEmptyList[CompilationError] \/ A)
        val (msgsB, resB) = f.run.run(msgsA) : (Set[CompilationMessageNonFatal], NonEmptyList[CompilationError] \/ (A => B))
        val res = (resA, resB) match {
          case (-\/(errA), -\/(errB)) => -\/(errA append errB)
          case (\/-(a), \/-(b)) => \/-(b(a))
          case (resA @ -\/(_), \/-(_)) => resA
          case (\/-(_), resB @ -\/(_)) => resB
        }
        (msgsB, res)
      })

    override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): StandardCompilationType[A] =
      fromStateF(prevMsgs => (prevMsgs ++ messages, \/-(value)))

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): StandardCompilationType[A] =
      fromStateF(prevMsgs => (prevMsgs ++ messages, -\/(errors)))

    override def map[A, B](fa: StandardCompilationType[A])(f: A => B): StandardCompilationType[B] =
      fa.map(f)

    override def bind[A, B](fa: StandardCompilationType[A])(f: A => StandardCompilationType[B]): StandardCompilationType[B] =
      fa.flatMap(f)
  }

}

/*



trait InnerValueHandler[F[_], -T, TResult] {
  def apply[T2 <: T](ft: F[T2]): TResult
}

sealed trait StandardCompilation[+T] {

  def withInnerValue[TResult](f: InnerValueHandler[CompilationImpl, T, TResult]): TResult



  def run: CompilerInternalError \/ (Vector[CompilationMessage], T) =
    withInnerValue(new InnerValueHandler[CompilationImpl, T, CompilerInternalError \/ (Vector[CompilationMessage], T)] {
      override def apply[T2 <: T](ft: CompilationImpl[T2]): \/[CompilerInternalError, (Vector[CompilationMessage], T)] =
        ft.run
    })

  final def map[U](f: T => U): StandardCompilation[U] =
    withInnerValue(new InnerValueHandler[CompilationImpl, T, StandardCompilation[U]] {
      override def apply[T2 <: T](ft: CompilationImpl[T2]): StandardCompilation[U] =
        StandardCompilation.fromCompilationImpl(ft.map(f))
    })

  final def flatMap[U](f: T => StandardCompilation[U]): StandardCompilation[U] =
    withInnerValue(new InnerValueHandler[CompilationImpl, T, StandardCompilation[U]] {
      override def apply[T2 <: T](ft: CompilationImpl[T2]): StandardCompilation[U] =
        StandardCompilation.fromCompilationImpl(
          ft.flatMap { value2 =>
            f(value2).withInnerValue(new InnerValueHandler[CompilationImpl, U, CompilationImpl[U]] {
              override def apply[U2 <: U](fu: CompilationImpl[U2]): CompilationImpl[U] =
                fu.map { res => res }
            })
          }
        )
    })

  def mapMessages[U](f: (Vector[CompilationMessage], T) => (Vector[CompilationMessage], U)): StandardCompilation[U] =
    withInnerValue(new InnerValueHandler[CompilationImpl, T, StandardCompilation[U]] {
      override def apply[T2 <: T](ft: CompilationImpl[T2]): StandardCompilation[U] =
        StandardCompilation.fromCompilationImpl(WriterT[CompilerInternalError \/ ?, Vector[CompilationMessage], U](ft.run.map(f.tupled)))
    })



}

object StandardCompilation {

  def fromCompilationImpl[T](innerValue: CompilationImpl[T]): StandardCompilation[T] =
    new StandardCompilation[T] {
      override def withInnerValue[TResult](f: InnerValueHandler[CompilationImpl, T, TResult]): TResult =
        f(innerValue)
    }

  def apply[T](value: T, compilationMessage: CompilationMessage*): StandardCompilation[T] =
    StandardCompilation.fromCompilationImpl(WriterT[CompilerInternalError \/ ?, Vector[CompilationMessage], T](\/-((Vector(compilationMessage: _*), value))))

  def internalError[T](error: CompilerInternalError): StandardCompilation[T] =
    fromCompilationImpl(WriterT[CompilerInternalError \/ ?, Vector[CompilationMessage], T](-\/(error)))


  implicit val monadInstance: Monad[StandardCompilation] = new Monad[StandardCompilation] {
  }

  implicit val compilationInstance: Compilation[StandardCompilation] = new Compilation[StandardCompilation] {


    override def create[A](value: A): StandardCompilation[A] = ???

    override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): StandardCompilation[A] = ???

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): StandardCompilation[A] = ???

    override def map[A, B](fa: StandardCompilation[A])(f: A => B): StandardCompilation[B] = ???

    override def combine[A, B, C](fa: StandardCompilation[A])(fb: StandardCompilation[B])(f: A => B => C): StandardCompilation[C] = ???

    override def forErrors[A](value: A, errors: CompilationMessage*): StandardCompilation[A] =
      StandardCompilation(value, errors: _*)
  }

}

final case class StandardCompilationT[F[_], T](innerValue: StandardCompilationHelpers.CompilationTImpl[F, T]) {

  def run(implicit fFunctor: Functor[F]): F[StandardCompilation[T]] =
    Functor[F].map(innerValue.run.run) { value =>
      StandardCompilation.fromCompilationImpl(WriterT[CompilerInternalError \/ ?, Vector[CompilationMessage], T](value))
    }

  def map[U](f: T => U)(implicit fFunctor: Functor[F]): StandardCompilationT[F, U] =
    new StandardCompilationT[F, U](innerValue.map(f))

  def flatMap[U](f: T => StandardCompilationT[F, U])(implicit fMonad: Monad[F]): StandardCompilationT[F, U] =
    new StandardCompilationT[F, U](innerValue.flatMap { value => f(value).innerValue })

}

object StandardCompilationT {

  final class CompilationTCreateHelper[F[_]](private val fApplicative: Applicative[F]) extends AnyVal {

    def fromValue[T](value: T, compilationMessage: CompilationMessage*): StandardCompilationT[F, T] =
      fromCompilation(StandardCompilation(value, compilationMessage: _*))

    def fromCompilation[T](value: StandardCompilation[T]): StandardCompilationT[F, T] =
      value.withInnerValue(new InnerValueHandler[CompilationImpl, T, StandardCompilationT[F, T]] {
        override def apply[T2 <: T](ft: CompilationImpl[T2]): StandardCompilationT[F, T] =
          forFCompilation(fApplicative.pure(ft.map[T] { t => t }.run))
      })

  }

  def create[F[_]: Applicative]: CompilationTCreateHelper[F] =
    new CompilationTCreateHelper[F](implicitly[Applicative[F]])


  def forFCompilation[F[_]: Applicative, T](value: F[StandardCompilation[T]]): StandardCompilationT[F, T] =
    forFCompilation(implicitly[Applicative[F]].map(value) { compValue =>
      compValue.withInnerValue(new InnerValueHandler[CompilationImpl, T, CompilerInternalError \/ (Vector[CompilationMessage], T)] {
        override def apply[T2 <: T](ft: CompilationImpl[T2]): CompilerInternalError \/ (Vector[CompilationMessage], T) =
          ft.map[T] { t => t }.run
      })
    })

  def forFCompilation[F[_], T](value: F[CompilerInternalError \/ (Vector[CompilationMessage], T)]): StandardCompilationT[F, T] =
    new StandardCompilationT[F, T](WriterT[EitherT[F, CompilerInternalError, ?], Vector[CompilationMessage], T](EitherT[F, CompilerInternalError, (Vector[CompilationMessage], T)](value)))

  def forF[F[_]: Applicative, T](value: F[T]): StandardCompilationT[F, T] =
    forFCompilation(implicitly[Applicative[F]].map(value) { a => StandardCompilation(a) })


  implicit def monadInstance[F[_]: Monad]: Monad[StandardCompilationT[F, ?]] = new Monad[StandardCompilationT[F, ?]] {
    override def point[A](a: => A): StandardCompilationT[F, A] =
      create[F].fromValue(a)

    override def bind[A, B](fa: StandardCompilationT[F, A])(f: A => StandardCompilationT[F, B]): StandardCompilationT[F, B] =
      StandardCompilationT(fa.innerValue.flatMap { a => f(a).innerValue })
  }

  implicit def compilationInstance[F[_] : Applicative]: Compilation[StandardCompilationT[F, ?]] = new Compilation[StandardCompilationT[F, ?]] {
    override def forErrors[A](value: A, errors: CompilationMessage*): StandardCompilationT[F, A] =
      create[F].fromCompilation(StandardCompilation(value, errors: _*))
  }

}

object StandardCompilationHelpers {
  type CompilationImpl[T] = WriterT[CompilerInternalError \/ ?, Vector[CompilationMessage], T]
  type CompilationTImpl[F[_], T] = WriterT[EitherT[F, CompilerInternalError, ?], Vector[CompilationMessage], T]

  def mapCompilationMessages[T, U](value: StandardCompilation[T])(f: ((Vector[CompilationMessage], T)) => (Vector[CompilationMessage], U)): StandardCompilation[U] =
    value.withInnerValue(new InnerValueHandler[CompilationImpl, T, StandardCompilation[U]] {
      override def apply[T2 <: T](ft: CompilationImpl[T2]): StandardCompilation[U] =
        StandardCompilation.fromCompilationImpl(
          WriterT[CompilerInternalError \/ ?, Vector[CompilationMessage], U](ft.run.map(f))
        )
    })

  def compileErrorCheck(condition: Boolean, message: => CompilationMessage): StandardCompilation[Unit] =
    if(condition)
      StandardCompilation((), message)
    else
      StandardCompilation(())

  implicit class CompilationOptionExtensions[T](val opt: Option[T]) extends AnyVal {
    def toCompilation(invalidValue: T, compilationMessages: CompilationMessage*): StandardCompilation[T] =
      opt.map(StandardCompilation(_)).getOrElse { StandardCompilation(invalidValue, compilationMessages: _*) }
  }

}*/