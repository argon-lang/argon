package dev.argon.io

type FileSystemResource[-R, +E] = BinaryResource[R, E] | DirectoryResource[R, E, BinaryResource]
