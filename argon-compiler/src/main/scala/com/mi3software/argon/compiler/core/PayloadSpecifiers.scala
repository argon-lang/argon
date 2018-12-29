package com.mi3software.argon.compiler.core

object PayloadSpecifiers {

  type DeclarationPayloadSpecifier[DeclPayload, RefPayload] = DeclPayload
  type ReferencePayloadSpecifier[DeclPayload, RefPayload] = RefPayload

}
