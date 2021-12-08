package dev.argon.compiler.signature

enum SignatureC[Type] {
  case Parameter(paramType: Type, next: SignatureC[Type])
  case Result(resultType: Type)
}

