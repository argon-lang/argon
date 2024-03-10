package dev.argon.plugin

import cats.data.OptionT
import dev.argon.compiler.DefinitionInfo
import dev.argon.esexpr.ESExprCodec
import zio.{Tag, ZIO}

trait ExternContext {
  type PlatformOptions[E >: PluginError]
  
  trait Extern {
    type Implementation
    given implementationCodec: ESExprCodec[Implementation]
    
    def loadExtern[E >: PluginError]
    (options: PlatformOptions[E])
    (id: String)
    : OptionT[[A] =>> ZIO[PluginEnv, E, A], Implementation]

    
    
    type Reference
    given referenceCodec: ESExprCodec[Reference]
    
    def defineReference[E >: PluginError]
    (options: PlatformOptions[E])
    (definitionInfo: DefinitionInfo)
    : ZIO[PluginEnv, E, Reference]
  }

  object Extern {
    trait Tagged extends Extern {
      given implementationTag: Tag[Implementation]
      given referenceTag: Tag[Reference]
    }
  }
}

