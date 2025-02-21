package dev.argon.backend.options

import cats.data.NonEmptySeq

enum OptionValue[E] {
  case Single(value: OptionValue.Atom[E])
  case ManyValues(values: NonEmptySeq[OptionValue.Atom[E]])
}

object OptionValue {
  enum Atom[E] {
    case String(s: java.lang.String)
    case Bool(b: Boolean)
    case BinaryResource(res: dev.argon.io.BinaryResource[E])
    case DirectoryResource(res: dev.argon.io.DirectoryResource[E, dev.argon.io.BinaryResource])
  }
  
  
  object ManyValues {
    def apply[E](head: Atom[E], tail: Atom[E]*): OptionValue[E] =
      OptionValue.ManyValues(NonEmptySeq(head, tail))
  }
  
}
