package dev.argon.backend.scalaApi

import java.io.IOException

final case class WrappingIOException[E](value: E) extends IOException
