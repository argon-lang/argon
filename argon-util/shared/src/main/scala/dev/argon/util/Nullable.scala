package dev.argon.util

opaque type Nullable[A] = A | Null

object Nullable:
  def apply[A](a: A | Null): Nullable[A] = a

  extension [A](a: Nullable[A])
    def unwrap: A | Null = a

    def map[B](f: A => B): Nullable[B] =
      if a == null then
        null
      else
        f(a.asInstanceOf[A])

    def fold[B](whenNull: => B, whenNotNull: A => B): B =
      if a == null then
        whenNull
      else
        whenNotNull(a.asInstanceOf[A])

    def toOption: Option[A] =
      fold(None, Some.apply)

  end extension

  given [A, B](using CanEqual[A, B]): CanEqual[Nullable[A], Nullable[A]] =
    CanEqual.derived

end Nullable
