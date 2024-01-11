package dev.argon.plugin

import dev.argon.esexpr.ESExprCodec

trait Extern {
  type Implementation
  given implementationCodec: ESExprCodec[Implementation]
  
  type Reference
  given referenceCodec: ESExprCodec[Reference]
  
  
}
