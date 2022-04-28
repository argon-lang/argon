package dev.argon.options

import scala.annotation.StaticAnnotation

final case class OptionMetadata(name: String, description: String) extends StaticAnnotation
