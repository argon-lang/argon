package dev.argon.plugin

import esexpr.ESExprCodec

final case class TubeLoaderName(plugin: String, loader: String) derives CanEqual, ESExprCodec
