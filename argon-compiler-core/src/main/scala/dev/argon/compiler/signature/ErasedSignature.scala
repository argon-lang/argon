package dev.argon.compiler.signature

import dev.argon.util.UniqueIdentifier

sealed trait ErasedSignature derives CanEqual
final case class ErasedSignatureWithResult(params: Seq[ErasedSignatureType], result: ErasedSignatureType) extends ErasedSignature derives CanEqual
final case class ErasedSignatureNoResult(params: Seq[ErasedSignatureType]) extends ErasedSignature derives CanEqual

enum ErasedSignatureType derives CanEqual {
  case Class(classImport: ImportSpecifier, args: Seq[ErasedSignatureType])
  case Trait(traitImport: ImportSpecifier, args: Seq[ErasedSignatureType])
  case Function(input: ErasedSignatureType, output: ErasedSignatureType)
  case Tuple(elements: Seq[ErasedSignatureType])
  case Erased
}


