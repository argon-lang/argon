package dev.argon.util

import java.util.Arrays
import java.util.Objects

object CharSequenceUtils {
  
  def concat(a: CharSequence, b: CharSequence): CharSequence =
    if a.isEmpty() then
      b
    else if b.isEmpty() then
      a
    else
      new CharSequence {
        override def subSequence(start: Int, end: Int): CharSequence =
          Objects.checkFromToIndex(start, end, length())
          if start > a.length() then
            b.subSequence(start - a.length(), end - a.length())
          else if end <= a.length() then
            a.subSequence(start, end)
          else
            concat(
              a.subSequence(start, a.length()),
              b.subSequence(0, end),
            )
        end subSequence

        override def charAt(index: Int): Char =
          Objects.checkIndex(index, length())
          if index < a.length() then
            a.charAt(index)
          else
            b.charAt(index - a.length())
        end charAt

        override def length(): Int =
          val len = a.length() + b.length()
          if len < 0 then throw new ArithmeticException("Concatenation of CharSequences resulted in overflow of length")
          len
        end length
      }

}
