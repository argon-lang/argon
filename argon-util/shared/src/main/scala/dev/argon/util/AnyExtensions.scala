package dev.argon.util


extension [A](a: A)
  def upcast[B >: A]: B = a
end extension

