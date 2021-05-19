package dev.argon.util

import cats.evidence.Is

sealed trait NList[N <: Nat, +A] {
  def +: [B >: A] (value: B): NList[Succ[N], B] =
    NCons(value, this)


  def asCons[P <: Nat](implicit lenIsSucc: N Is Succ[P]): NCons[P, A]


  def iterator: Iterator[A] = new Iterator[A] {
    @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
    private var list: NList[_, A] = NList.this

    override def hasNext: Boolean =
      list match {
        case NNil => false
        case NCons(_, _) => true
      }

    override def next(): A =
      list match {
        case NNil => throw new NoSuchElementException()
        case NCons(head, tail) =>
          list = tail
          head
      }
  }

  def toIterable: Iterable[A] = new Iterable[A] {
    override def iterator: Iterator[A] = NList.this.iterator
  }

  def toSeq: Seq[A] = toIterable.toSeq
  def toVector: Vector[A] = toIterable.toVector

}
case object NNil extends NList[Zero, Nothing] {
  override def asCons[P <: Nat](implicit lenIsSucc: Zero Is Succ[P]): NCons[P, Nothing] =
    IsHelpers.absurd[Zero, Succ[P]](Zero)

}
final case class NCons[P <: Nat, +A](head: A, tail: NList[P, A]) extends NList[Succ[P], A] {
  override def asCons[P2 <: Nat](implicit lenIsSucc: Succ[P] Is Succ[P2]): NCons[P2, A] = {
    type BindCons[P3 <: Nat] = NCons[P3, A]
    implicit val pIsP2: P Is P2 = IsHelpers.unwrapBounded(lenIsSucc)
    IsHelpers.substituteBounded[Nat, Nothing, BindCons, P, P2](pIsP2)(this)
  }

}

object NList {
  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def sized[N <: Nat: Nat.ToInt, A](s: Iterable[A]): Option[NList[N, A]] =
    sizedImpl(implicitly[Nat.ToInt[N]].value)(s.iterator).asInstanceOf[Option[NList[N, A]]]

  private def sizedImpl[N <: Nat, A](n: Int)(s: Iterator[A]): Option[NList[_, A]] =
    if(n > 0) {
      if(s.hasNext) {
        val value = s.next()
        sizedImpl(n - 1)(s).map { t => value +: t }
      }
      else {
        None
      }
    }
    else if(s.hasNext) None
    else Some(NNil)

  implicit class NListOps[N <: Nat, A](private val list: NList[N, A]) extends AnyVal {
    def head[P <: Nat](implicit lenIsSucc: N Is Succ[P]): A = list.asCons.head
    def tail[P <: Nat](implicit lenIsSucc: N Is Succ[P]): NList[P, A] = list.asCons.tail
  }
}





