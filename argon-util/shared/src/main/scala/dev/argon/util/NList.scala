package dev.argon.util

sealed trait NList[N <: Nat, +A] derives CanEqual:
  def +:[B >: A](value: B): NList[Succ[N], B] = NCons(value, this)

  def asCons[P <: Nat](using N Is Succ[P]): NCons[P, A]

  def zip[B](other: NList[N, B]): NList[N, (A, B)]
  def map[B](f: A => B): NList[N, B]

  def iterator: Iterator[A] =
    new Iterator[A] {

      @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
      private var list: NList[?, A] = NList.this

      override def hasNext: Boolean =
        list match {
          case _: NNil.type => false
          case NCons(_, _) => true
        }

      override def next(): A =
        list match {
          case _: NNil.type => throw new NoSuchElementException()
          case NCons(head, tail) =>
            list = tail
            head
        }

    }

  def toIterable: Iterable[A] =
    new Iterable[A] {
      override def iterator: Iterator[A] = NList.this.iterator
    }

  def toSeq: Seq[A] = toIterable.toSeq
  def toVector: Vector[A] = toIterable.toVector

  def makeFactory: NListFactory[N]
end NList

case object NNil extends NList[Zero, Nothing] {
  override def asCons[P <: Nat](using zeroIsSucc: Zero Is Succ[P]): NCons[P, Nothing] = Is.nothing(zeroIsSucc.both(Zero))

  override def zip[B](other: NList[Zero, B]): NList[Zero, (Nothing, B)] = NNil
  override def map[B](f: Nothing => B): NList[Zero, B] = NNil

  override def makeFactory: NListFactory[Zero] = summon[NListFactory[Zero]]
}

final case class NCons[P <: Nat, +A](head: A, tail: NList[P, A]) extends NList[Succ[P], A] {

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  override def asCons[P2 <: Nat](implicit lenIsSucc: Succ[P] Is Succ[P2]): NCons[P2, A] = {
    val pIsP2 = Is.refl[P].asInstanceOf[P Is P2]
    pIsP2.substituteBounded[Nat, Nothing, [P3 <: Nat] =>> NCons[P3, A]](this)
  }

  override def zip[B](other: NList[Succ[P], B]): NList[Succ[P], (A, B)] = NCons((head, other.head), tail.zip(other.tail))

  override def map[B](f: A => B): NList[Succ[P], B] = NCons(f(head), tail.map(f))

  override def makeFactory: NListFactory[Succ[P]] = nListFactorySucc(using tail.makeFactory)
}

object NList {

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def sized[N <: Nat : Nat.ToInt, A](s: Iterable[A]): Option[NList[N, A]] =
    sizedImpl(implicitly[Nat.ToInt[N]].value)(s.iterator).asInstanceOf[Option[NList[N, A]]]

  def from[A](s: Seq[A]): NList[?, A] =
    s match {
      case h +: t => NCons(h, from(t))
      case _ => NNil
    }

  private def sizedImpl[N <: Nat, A](n: Int)(s: Iterator[A]): Option[NList[?, A]] =
    if n > 0 then {
      if s.hasNext then {
        val value = s.next()
        sizedImpl(n - 1)(s).map { t => value +: t }
      }
      else {
        None
      }
    }
    else if s.hasNext then None
    else Some(NNil)

  implicit class NListOps[N <: Nat, A](private val list: NList[N, A]) extends AnyVal {
    def head[P <: Nat](implicit lenIsSucc: N Is Succ[P]): A = list.asCons.head
    def tail[P <: Nat](implicit lenIsSucc: N Is Succ[P]): NList[P, A] = list.asCons.tail
  }

}

sealed trait NListFactory[N <: Nat] {
  def fromSeqPrefix[A](s: Seq[A]): Option[(NList[N, A], Seq[A])]

  final def fromSeq[A](s: Seq[A]): Option[NList[N, A]] =
    fromSeqPrefix(s).flatMap {
      case (nlist, Seq()) => Some(nlist)
      case _ => None
    }

}

given NListFactory[Zero] with
  override def fromSeqPrefix[A](s: Seq[A]): Option[(NList[Zero, A], Seq[A])] = Some(NNil, s)
end given

given nListFactorySucc[P <: Nat : NListFactory]: NListFactory[Succ[P]] with {

  override def fromSeqPrefix[A](s: Seq[A]): Option[(NList[Succ[P], A], Seq[A])] =
    s match {
      case h +: t =>
        summon[NListFactory[P]].fromSeqPrefix(t).map { case (tSized, rest) =>
          (NCons(h, tSized), rest)
        }
      case _ => None
    }

}
