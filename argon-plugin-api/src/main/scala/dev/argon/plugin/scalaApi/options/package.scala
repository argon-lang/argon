package dev.argon.plugin.scalaApi.options

type OptionDecodePath = esexpr.ESExprCodec.ErrorPath
type BinaryResource[E] = dev.argon.io.BinaryResource[E]
type DirectoryResource[E] = dev.argon.io.DirectoryResource[E, dev.argon.io.BinaryResource]
