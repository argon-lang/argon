package dev.argon.util

import scalaz._
import Scalaz._

import scala.reflect.ClassTag

final case class NonEmptyVector[+A](head: A, tail: Vector[A]) {

  def size: Int = tail.size + 1
  def toVector: Vector[A] = head +: tail
  def toArray[B >: A : ClassTag]: Array[B] = toVector.toArray

  def append[B >: A](other: NonEmptyVector[B]): NonEmptyVector[B] =
    append(other.toVector)

  def append[B >: A](other: Vector[B]): NonEmptyVector[B] =
    NonEmptyVector(head, tail ++ other)

  def prepend[B >: A](other: Vector[B]): NonEmptyVector[B] =
    other match {
      case otherHead +: otherTail => NonEmptyVector(otherHead, otherTail ++ toVector)
      case Vector() => this
    }

  def splitAt(index: Int): (NonEmptyVector[A], Vector[A]) =
    if(index < 1)
      (NonEmptyVector(head, Vector.empty), tail)
    else if(index >= size)
      (this, Vector.empty)
    else {
      val (p1, p2) = tail.splitAt(index - 1)
      (NonEmptyVector(head, p1), p2)
    }

  def map[B](f: A => B): NonEmptyVector[B] =
    NonEmptyVector(f(head), tail.map(f))

  def flatMap[B](f: A => NonEmptyVector[B]): NonEmptyVector[B] =
    f(head).append(tail.flatMap { a => f(a).toVector })

}

object NonEmptyVector {

  def of[A](head: A, tail: A*): NonEmptyVector[A] = NonEmptyVector(head, tail.toVector)

  implicit val foldableInstance: Foldable1[NonEmptyVector] = new Foldable1[NonEmptyVector] {
    override def foldMap1[A, B](fa: NonEmptyVector[A])(f: A => B)(implicit F: Semigroup[B]): B =
      fa.tail.foldLeft(f(fa.head)) { (b, a) => F.append(b, f(a)) }


    override def foldMapRight1[A, B](fa: NonEmptyVector[A])(z: A => B)(f: (A, => B) => B): B =
      fa match {
        case NonEmptyVector(head, mid :+ end) => f(head, Foldable[Vector].foldRight(mid, z(end))(f))
        case NonEmptyVector(head, Vector()) => z(head)
      }

    override def foldMapLeft1[A, B](fa: NonEmptyVector[A])(z: A => B)(f: (B, A) => B): B =
      fa.tail.foldLeft(z(fa.head))(f)

  }

  implicit val monadInstance: Monad[NonEmptyVector] = new Monad[NonEmptyVector] {
    override def point[A](a: => A): NonEmptyVector[A] = NonEmptyVector(a, Vector.empty)

    override def bind[A, B](fa: NonEmptyVector[A])(f: A => NonEmptyVector[B]): NonEmptyVector[B] =
      fa.flatMap(f)

    override def map[A, B](fa: NonEmptyVector[A])(f: A => B): NonEmptyVector[B] =
      fa.map(f)
  }

}
