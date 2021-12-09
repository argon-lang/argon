package dev.argon.util

type Id[+A] = A

given Monad[Id] with
  override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  override def pure[A](a: A): Id[A] = a

  override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  override def ap[A, B](fa: Id[A])(fab: Id[A => B]): Id[B] = fab(fa)
end given
