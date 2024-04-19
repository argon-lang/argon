package dev.argon.compiler

import dev.argon.expr.BuiltinType
import dev.argon.ast.IdentifierExpr


final case class ErasedSignature(params: Seq[ErasedSignatureType], result: ErasedSignatureType) derives CanEqual

enum ErasedSignatureType derives CanEqual {
  case Builtin(builtin: BuiltinType, args: Seq[ErasedSignatureType])
  case Function(input: ErasedSignatureType, output: ErasedSignatureType)
  case Record(recordImport: ImportSpecifier, args: Seq[ErasedSignatureType])
  case Tuple(elements: Seq[ErasedSignatureType])
  case Erased
}

final case class ImportSpecifier(
  tube: TubeName,
  module: ModulePath,
  name: Option[IdentifierExpr],
  signature: ErasedSignature,
)
