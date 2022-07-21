package dev.argon.io

enum DirectoryEntry[-R, +E, +FileResource[-R2, +E2] <: Resource[R2, E2]] {
  case Subdirectory(name: String, resource: DirectoryResource[R, E, FileResource])
  case File(name: String, resource: FileResource[R, E])
}

