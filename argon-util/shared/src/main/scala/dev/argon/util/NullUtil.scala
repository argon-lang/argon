package dev.argon.util

extension [A](value: A | Null)
  def isNull: Boolean =
    given CanEqual[A | Null, Null] = CanEqual.derived
    value == null
  end isNull

end extension

