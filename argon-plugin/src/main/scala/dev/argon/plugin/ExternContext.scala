package dev.argon.plugin

import cats.data.OptionT
import dev.argon.compiler.DefinitionInfo
import esexpr.ESExprCodec
import zio.{Tag, ZIO}

trait ExternContext[E >: PluginError] {
  type PlatformOptions
  

  trait ExternRef {
    type Reference
    given referenceCodec: ESExprCodec[Reference]
    
    def defineReference
    (options: PlatformOptions)
    (definitionInfo: DefinitionInfo)
    : ZIO[PluginEnv, E, Reference]
  }


  trait Extern extends ExternRef {
    type Implementation
    given implementationCodec: ESExprCodec[Implementation]
    
    def loadExtern
    (options: PlatformOptions)
    (id: String)
    : OptionT[[A] =>> ZIO[PluginEnv, E, A], Implementation]
  }

  object Extern {
    trait TaggedRef extends ExternRef {
      given referenceTag: Tag[Reference]
    }

    trait Tagged extends Extern with TaggedRef {
      given implementationTag: Tag[Implementation]
    }
  }
}

