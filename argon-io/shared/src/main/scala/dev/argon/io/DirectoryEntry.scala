package dev.argon.io

final case class DirectoryEntry[+E, +FileResource[+E2] <: Resource[E2]](dirs: Seq[String], fileName: String, resource: FileResource[E])

