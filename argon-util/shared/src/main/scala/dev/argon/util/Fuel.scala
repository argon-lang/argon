package dev.argon.util

final class Fuel(amount: Int) {
  def isEmpty: Boolean = amount <= 0
  def nonEmpty: Boolean = !isEmpty
  def consume: Fuel =
    if isEmpty then
      this
    else
      Fuel(amount - 1)
}
