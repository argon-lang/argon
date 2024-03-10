package dev.argon.plugin

import dev.argon.esexpr.ESExprCodec

final case class TubeLoaderName(plugin: String, loader: String) derives CanEqual, ESExprCodec
