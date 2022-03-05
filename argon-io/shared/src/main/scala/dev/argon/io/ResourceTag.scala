package dev.argon.io

sealed class ResourceTag[+T <: Resource]

class DirectoryResourceTag[+T <: DirectoryResource] extends ResourceTag[T]

class BinaryResourceTag[+T <: BinaryResource] extends ResourceTag[T]

class TextResourceTag[+T <: TextResource] extends BinaryResourceTag[T]

class ZipFileResourceTag[+T <: ZipFileResource] extends BinaryResourceTag[T]

