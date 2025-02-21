package dev.argon.backend.options

import zio.*

trait OptionParser[E, Options] {
  def parse(options: Map[String, OptionValue[E]]): IO[OptionParseFailure, Options]
}
