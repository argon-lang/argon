package dev.argon.util.builder

import cats._

final case class BuilderString[A](run: StringBuilder => Eval[A]) {

  def map[B](f: A => B): BuilderString[B] =
    BuilderString { sb => run(sb).map(f) }

  def flatMap[B](f: A => BuilderString[B]): BuilderString[B] =
    BuilderString { sb =>
      run(sb).flatMap { a => f(a).run(sb) }
    }

  def build: Eval[(String, A)] = {
    val sb = new StringBuilder()
    Eval.defer(run(sb)).map { a => (sb.toString, a) }
  }


}

object BuilderString {

  def pure[A](a: A): BuilderString[A] = BuilderString { _ => Eval.now(a) }

  implicit val builderStringBuilderInstance: Builder[BuilderString, String] = new Builder[BuilderString, String] with StackSafeMonad[BuilderString] {
    override def append(value: String): BuilderString[Unit] = BuilderString { sb => Eval.always { val _ = sb.append(value); () } }

    override def flatMap[A, B](fa: BuilderString[A])(f: A => BuilderString[B]): BuilderString[B] =
      fa.flatMap(f)

    override def map[A, B](fa: BuilderString[A])(f: A => B): BuilderString[B] =
      fa.map(f)

    override def pure[A](x: A): BuilderString[A] = BuilderString.pure(x)
  }

}
