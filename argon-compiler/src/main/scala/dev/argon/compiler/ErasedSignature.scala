package dev.argon.compiler

import dev.argon.expr.BuiltinType
import dev.argon.ast.IdentifierExpr
import dev.argon.util.UniqueIdentifier


final case class ErasedSignature(params: Seq[ErasedSignatureType], result: ErasedSignatureType) derives CanEqual

enum ErasedSignatureType derives CanEqual {
  case Builtin(builtin: BuiltinType, args: Seq[ErasedSignatureType])
  case Function(input: ErasedSignatureType, output: ErasedSignatureType)
  case Record(recordImport: ImportSpecifier, args: Seq[ErasedSignatureType])
  case Tuple(elements: Seq[ErasedSignatureType])
  case Erased
}

enum ImportSpecifier derives CanEqual {
  case Global(
    tube: TubeName,
    module: ModulePath,
    name: IdentifierExpr,
    signature: ErasedSignature,
  )
  case Local(
    parent: ImportSpecifier,
    id: UniqueIdentifier,
  )
}
