package dev.argon.util

import java.util.Objects

/** A rope/cord representation of character data. Leaves reference existing CharSequences.
  * The structure is immutable and implements CharSequence.
  */
sealed trait Cord extends CharSequence {
  override def length: Int
  override def charAt(index: Int): Char
  override def subSequence(start: Int, end: Int): CharSequence

  /** Concatenate two cords (or CharSequences) efficiently without copying. */
  def ++(other: CharSequence): Cord = Cord.concat(this, Cord.from(other))

  /** Convert this Cord to a String by flattening. */
  override def toString: String = {
    val sb = new StringBuilder(length())
    Cord.foreachLeaf(this) { cs => sb.append(cs) }
    sb.toString()
  }
}

object Cord {
  /** Construct a leaf Cord from a CharSequence. If the input is already a Cord, it is returned. */
  def from(cs: CharSequence): Cord = cs match {
    case _ if cs.length() == 0 => empty
    case c: Cord => c
    case _ => Leaf(cs)
  }

  /** Empty Cord singleton */
  val empty: Cord = Leaf("")

  /** Efficient concatenation that maintains a balanced tree where possible. */
  def concat(a: CharSequence, b: CharSequence): Cord = (from(a), from(b)) match {
    case (Leaf(x), _) if x.length() == 0 => from(b)
    case (_, Leaf(y)) if y.length() == 0 => from(a)
    case (l: Leaf, r: Leaf) => Node(l, r, safeAdd(l.length(), r.length()))
    case (l, r) => Node(l, r, safeAdd(l.length(), r.length()))
  }

  private final case class Leaf(value: CharSequence) extends Cord {
    override def length: Int = value.length()
    override def charAt(index: Int): Char = {
      if index < 0 || index >= length() then
        throw new IndexOutOfBoundsException()

      value.charAt(index)
    }
    override def subSequence(start: Int, end: Int): CharSequence = {
      if start < 0 || start > end || end > length() then
        throw new IndexOutOfBoundsException()

      val sub = value.subSequence(start, end)
      Leaf(sub)
    }
    override def toString: String = value.toString
  }

  private final case class Node(left: Cord, right: Cord, len: Int) extends Cord {
    override def length: Int = len
    override def charAt(index: Int): Char = {
      if index < 0 || index >= length() then
        throw new IndexOutOfBoundsException()

      val lLen = left.length()
      if index < lLen then left.charAt(index) else right.charAt(index - lLen)
    }
    override def subSequence(start: Int, end: Int): CharSequence = {
      if start < 0 || start > end || end > len then
        throw new IndexOutOfBoundsException()
      if start == 0 && end == len then this
      else {
        val lLen = left.length()
        if end <= lLen then left.subSequence(start, end)
        else if start >= lLen then right.subSequence(start - lLen, end - lLen)
        else {
          val leftSub = left.subSequence(start, lLen)
          val rightSub = right.subSequence(0, end - lLen)
          concat(leftSub, rightSub)
        }
      }
    }
  }

  private def foreachLeaf(c: Cord)(f: CharSequence => Unit): Unit = c match {
    case Leaf(v) => f(v)
    case Node(l, r, _) =>
      foreachLeaf(l)(f)
      foreachLeaf(r)(f)
  }

  private def safeAdd(a: Int, b: Int): Int = {
    val res = a + b
    if res < 0 then throw new ArithmeticException("Cord length overflow")
    res
  }
}
