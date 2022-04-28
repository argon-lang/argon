package dev.argon.options

import dev.argon.io.Resource

trait OutputHandler[E, Output] {
  val options: Set[OutputInfo[_ <: Resource[E], Output]]
}
