package com.mi3software.argon.compiler

object PayloadSpecifiers {

  type DeclarationPayloadSpecifier[DeclPayload, RefPayload] = DeclPayload
  type ReferencePayloadSpecifier[DeclPayload, RefPayload] = RefPayload

}
