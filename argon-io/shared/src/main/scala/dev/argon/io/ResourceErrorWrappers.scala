package dev.argon.io

import zio.stream.*
import dev.argon.util.async.ErrorWrapper

final class BinaryResourceErrorWrapped[E, EX <: Throwable](res: BinaryResource[E])(using ErrorWrapper.Aux[E, EX]) extends BinaryResource[EX] {
  override def asBytes: Stream[EX, Byte] =
    ErrorWrapper.wrapStream(res.asBytes)

  override def fileName: Option[String] =
    res.fileName
}

final class BinaryResourceErrorUnwrapped[E, EX <: Throwable](res: BinaryResource[EX])(using ErrorWrapper.Aux[E, EX]) extends BinaryResource[E] {
  override def asBytes: Stream[E, Byte] =
    ErrorWrapper.unwrapStream(res.asBytes)

  override def fileName: Option[String] =
    res.fileName
}

final class DirectoryResourceErrorWrapped[E, EX <: Throwable](res: DirectoryResource[E, BinaryResource])(using ErrorWrapper.Aux[E, EX]) extends DirectoryResource[EX, BinaryResource] {
  override def contents: Stream[EX, DirectoryEntry[EX, BinaryResource]] =
    ErrorWrapper.wrapStream(res.contents)
      .map {
        case DirectoryEntry.File(name, resource) =>
          DirectoryEntry.File(name, BinaryResourceErrorWrapped(resource))

        case DirectoryEntry.Subdirectory(name, resource) =>
          DirectoryEntry.Subdirectory(name, DirectoryResourceErrorWrapped(resource))
      }

  override def fileName: Option[String] =
    res.fileName
}

final class DirectoryResourceErrorUnwrapped[E, EX <: Throwable](res: DirectoryResource[EX, BinaryResource])(using ErrorWrapper.Aux[E, EX]) extends DirectoryResource[E, BinaryResource] {
  override def contents: Stream[E, DirectoryEntry[E, BinaryResource]] =
    ErrorWrapper.unwrapStream(res.contents)
      .map {
        case DirectoryEntry.File(name, resource) =>
          DirectoryEntry.File(name, BinaryResourceErrorUnwrapped(resource))

        case DirectoryEntry.Subdirectory(name, resource) =>
          DirectoryEntry.Subdirectory(name, DirectoryResourceErrorUnwrapped(resource))
      }

  override def fileName: Option[String] =
    res.fileName
}
