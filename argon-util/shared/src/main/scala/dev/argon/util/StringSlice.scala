package dev.argon.util

final case class StringSlice private(s: String, start: Int, end: Int) extends CharSequence {
  override def length(): Int = end - start
  override def charAt(index: Int): Char = s.charAt(start + index)
  override def subSequence(start: Int, end: Int): CharSequence =
    if start < 0 || start > end || end > length() then
      throw new IndexOutOfBoundsException()

    new StringSlice(s, this.start + start, this.start + end)
  end subSequence

  override def toString: String = s.substring(start, end)
}

object StringSlice {
  def apply(s: String): StringSlice = StringSlice(s, 0, s.length)
  def apply(s: String, start: Int, end: Int): StringSlice =
    if start < 0 || start > end || end > s.length() then
      throw new IndexOutOfBoundsException()

    new StringSlice(s, start, end)
  end apply
}
