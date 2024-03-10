package dev.argon.compiler

import dev.argon.expr.BuiltinType


final case class ErasedSignature(params: Seq[ErasedSignatureType], result: ErasedSignatureType) derives CanEqual

enum ErasedSignatureType derives CanEqual {
  case Builtin(builtin: BuiltinType, args: Seq[ErasedSignatureType])
  case Function(input: ErasedSignatureType, output: ErasedSignatureType)
  case Tuple(elements: Seq[ErasedSignatureType])
  case Erased
}
