package dev.argon.io

enum DirectoryEntry[+E, +FileResource[+E2] <: Resource[E2]] {
  val name: String

  case Subdirectory(name: String, resource: DirectoryResource[E, FileResource])
  case File(name: String, resource: FileResource[E])
}

