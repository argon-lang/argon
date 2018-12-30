package com.mi3software.argon.compiler

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
