package com.mi3software.argon.compiler

import scalaz._

sealed trait Lookup[T] {
  def next[U](cmp: LookupComparer[T])(f: T => Lookup[U]): Lookup[U]
  def resolve(cmp: LookupComparer[T]): LookupResult[T]
  def resolveAll(cmp: LookupComparer[T]): LookupResult[T]
}

object Lookup {

  final class Single[T](value: T) extends Lookup[T] {
    override def next[U](cmp: LookupComparer[T])(f: T => Lookup[U]): Lookup[U] =
      f(value)

    override def resolve(cmp: LookupComparer[T]): LookupResult[T] =
      LookupResult.Success(value, Vector.empty).filterNot(cmp.hasError)

    override def resolveAll(cmp: LookupComparer[T]): LookupResult[T] =
      resolve(cmp)
  }

  private def groupValues[T](cmp: LookupComparer[T])(values: Vector[T]): Vector[NonEmptyList[T]] =
    values.foldLeft(Vector.empty[NonEmptyList[T]]) { (acc, value) =>

      def insertHelper(prev: Vector[NonEmptyList[T]], acc: Vector[NonEmptyList[T]]): Vector[NonEmptyList[T]] =
        acc match {
          case (current @ NonEmptyList(first, _)) +: tail =>
            cmp.compare(value, first) match {
              case LookupComparison.BetterThan => prev ++ (NonEmptyList(value) +: acc)
              case LookupComparison.SameAs => prev ++ ((value <:: current) +: tail)
              case LookupComparison.WorseThan => insertHelper(prev :+ current, tail)
            }

          case Vector() =>
            prev :+ NonEmptyList(value)
        }

      insertHelper(Vector.empty, acc)
    }

  private def findBestResult[T](cmp: LookupComparer[T])(values: NonEmptyList[T]): LookupResult[T] = {

    def impl(bestValues: NonEmptyList[T], rejects: Vector[T], remaining: IList[T]): LookupResult[T] =
      remaining match {
        case ICons(head, tail) =>
          cmp.compare(bestValues.head, head) match {
            case LookupComparison.BetterThan => impl(bestValues, rejects :+ head, tail)
            case LookupComparison.SameAs => impl(head <:: bestValues, rejects, tail)
            case LookupComparison.WorseThan => impl(NonEmptyList(head), rejects ++ bestValues.list.toVector, tail)
          }

        case INil() =>
          bestValues match {
            case NonEmptyList(head, INil()) =>
              LookupResult.Success(head, rejects)

            case NonEmptyList(head1, ICons(head2, tail)) =>
              LookupResult.Ambiguity(head1, head2, tail.toVector, rejects)
          }
      }

    impl(NonEmptyList(values.head), Vector.empty, values.tail)
  }

  final class OneOf[T](values: Vector[T]) extends Lookup[T] {
    override def next[U](cmp: LookupComparer[T])(f: T => Lookup[U]): Lookup[U] =
      new OneOfNext(values, f, cmp)

    override def resolve(cmp: LookupComparer[T]): LookupResult[T] =
      groupValues(cmp)(values).foldLeft(LookupResult.Failure(Vector.empty) : LookupResult[T]) { (prevResult, group) =>
        prevResult.orElse(findBestResult(cmp)(group))
      }

    override def resolveAll(cmp: LookupComparer[T]): LookupResult[T] =
      LookupResult.fromResults(values, Vector.empty).filterNot(cmp.hasError)
  }

  final class OneOfNext[A, B](prevValues: Vector[A], prevFunc: A => Lookup[B], prevCmp: LookupComparer[A]) extends Lookup[B] {
    override def next[U](cmp: LookupComparer[B])(f: B => Lookup[U]): Lookup[U] =
      new OneOfNext[A, U](prevValues, a => prevFunc(a).next(cmp)(f), prevCmp)

    override def resolve(cmp: LookupComparer[B]): LookupResult[B] =
      groupValues(prevCmp)(prevValues).foldLeft(LookupResult.Failure(Vector.empty) : LookupResult[B]) { (prevResult, group) =>
        val groupResults = LookupResult.combine(group.list.toVector.map { a => prevFunc(a).resolveAll(cmp) })

        val groupResult = groupResults match {
          case LookupResult.Success(value, rejects) => LookupResult.Success(value, rejects)
          case LookupResult.Ambiguity(item1, item2, rest, rejects) => findBestResult(cmp)(NonEmptyList(item1, item2 +: rest: _*)).addRejects(rejects)
          case LookupResult.Failure(rejects) => LookupResult.Failure(rejects)
        }

        prevResult.orElse(groupResult)
      }

    override def resolveAll(cmp: LookupComparer[B]): LookupResult[B] =
      LookupResult.combine(prevValues.map { p => prevFunc(p).resolveAll(cmp) })
  }

  final class Alternative[T](first: Lookup[T], second: => Lookup[T]) extends Lookup[T] {
    override def next[U](cmp: LookupComparer[T])(f: T => Lookup[U]): Lookup[U] =
      new Alternative(first.next(cmp)(f), second.next(cmp)(f))

    override def resolve(cmp: LookupComparer[T]): LookupResult[T] =
      first.resolve(cmp).orElse(second.resolve(cmp))

    override def resolveAll(cmp: LookupComparer[T]): LookupResult[T] =
      first.resolveAll(cmp).orElse(second.resolveAll(cmp))
  }

}

trait LookupComparer[T] {
  def hasError(a: T): Boolean
  def compare(a: T, b: T): LookupComparison
}

sealed trait LookupComparison
object LookupComparison {
  case object BetterThan extends LookupComparison
  case object SameAs extends LookupComparison
  case object WorseThan extends LookupComparison
}

sealed trait LookupResult[T] {

  def orElse(other: => LookupResult[T]): LookupResult[T]

  def filter(f: T => Boolean): LookupResult[T]

  def filterNot(f: T => Boolean): LookupResult[T] =
    filter(a => !f(a))

  def flatMap[U](f: T => LookupResult[U]): LookupResult[U]

  def rejectAll: Vector[T]
  def addRejects(newRejects: Vector[T]): LookupResult[T]

  def ++(other: LookupResult[T]): LookupResult[T]

}
object LookupResult {

  def empty[T]: LookupResult[T] = Failure(Vector.empty)

  def combine[T](items: Vector[LookupResult[T]]): LookupResult[T] =
    items.foldLeft(LookupResult.empty[T]) { _ ++ _ }

  def fromResults[T](results: Vector[T], rejects: Vector[T]): LookupResult[T] =
    results match {
      case head1 +: head2 +: tail => Ambiguity(head1, head2, tail, rejects)
      case head +: Vector() => Success(head, rejects)
      case Vector() => Failure(rejects)
    }

  final case class Success[T](value: T, rejects: Vector[T]) extends LookupResult[T] {
    override def orElse(other: => LookupResult[T]): LookupResult[T] = this

    override def filter(f: T => Boolean): LookupResult[T] =
      if(f(value))
        this
      else
        Failure(rejectAll)

    override def flatMap[U](f: T => LookupResult[U]): LookupResult[U] =
      f(value).addRejects(rejects.flatMap { reject => f(reject).rejectAll })

    override def rejectAll: Vector[T] = value +: rejects

    override def addRejects(newRejects: Vector[T]): LookupResult[T] =
      Success(value, rejects ++ newRejects)

    override def ++(other: LookupResult[T]): LookupResult[T] =
      other match {
        case Success(otherValue, otherRejects) => Ambiguity(value, otherValue, Vector.empty, rejects ++ otherRejects)
        case Failure(otherRejects) => addRejects(otherRejects)
        case Ambiguity(item1, item2, rest, otherRejects) => Ambiguity(value, item1, item2 +: rest, rejects ++ otherRejects)
      }
  }

  final case class Failure[T](rejects: Vector[T]) extends LookupResult[T] {
    override def orElse(other: => LookupResult[T]): LookupResult[T] =
      other.addRejects(rejects)

    override def filter(f: T => Boolean): LookupResult[T] =
      this

    override def flatMap[U](f: T => LookupResult[U]): LookupResult[U] =
      Failure(rejects.flatMap { reject => f(reject).rejectAll })

    override def rejectAll: Vector[T] =
      rejects

    override def addRejects(newRejects: Vector[T]): LookupResult[T] =
      Failure(rejects ++ newRejects)

    override def ++(other: LookupResult[T]): LookupResult[T] =
      other.addRejects(rejects)
  }

  final case class Ambiguity[T](item1: T, item2: T, rest: Vector[T], rejects: Vector[T]) extends LookupResult[T] {
    override def orElse(other: => LookupResult[T]): LookupResult[T] =
      this

    override def filter(f: T => Boolean): LookupResult[T] =
      values.partition(f) match {
        case (head1 +: head2 +: tail, newRejects) => Ambiguity(head1, head2, tail, rejects ++ newRejects)
        case (head1 +: Vector(), newRejects) => Success(head1, rejects ++ newRejects)
        case (Vector(), newRejects) => Failure(rejects ++ newRejects)
      }

    override def flatMap[U](f: T => LookupResult[U]): LookupResult[U] =
      combine(values.map(f))

    override def rejectAll: Vector[T] = (item1 +: item2 +: rest) ++ rejects

    override def addRejects(newRejects: Vector[T]): LookupResult[T] =
      Ambiguity(item1, item2, rest, rejects)

    override def ++(other: LookupResult[T]): LookupResult[T] =
      other match {
        case Success(otherValue, otherRejects) => Ambiguity(item1, item2, rest :+ otherValue, rejects ++ otherRejects)
        case Failure(otherRejects) => addRejects(otherRejects)
        case other @ Ambiguity(_, _, _, otherRejects) => Ambiguity(item1, item2, rest ++ other.values, rejects ++ otherRejects)
      }

    private def values: Vector[T] = item1 +: item2 +: rest
  }
}

