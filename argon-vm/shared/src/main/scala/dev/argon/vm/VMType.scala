package dev.argon.vm

enum VMType derives CanEqual {
  case I8, I16, I32, I64
  case F32, F64
  case GCRef
  case GCPtr
  case Tuple(elements: Seq[VMType])
}
