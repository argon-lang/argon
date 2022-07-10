package dev.argon.options

import dev.argon.io.Resource

trait OutputHandler[Output[_]] {
  val options: Set[OutputInfoAny[Output]]
}
