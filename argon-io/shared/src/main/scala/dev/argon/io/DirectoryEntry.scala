package dev.argon.io

import cats.data.NonEmptySeq

final case class DirectoryEntry[+E, +FileResource[+E2] <: Resource[E2]](dirs: Seq[String], fileName: String, resource: FileResource[E])

