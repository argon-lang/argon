package dev.argon.io

import java.nio.file.Path

type PathLike = Path

object PathLike {
  def fromString(s: String): PathLike = Path.of(s).nn
  def join(a: PathLike, b: String): PathLike = a.resolve(b).nn
}

