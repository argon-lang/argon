package dev.argon

import cats.evidence.===
import dev.argon.io.Path

package object platform {
  type FilePath = FilePathImpl.filePathInfo.FilePath
  implicit val pathInstance: Path[FilePath] = FilePathImpl.filePathInfo.pathInstance
  private[platform] implicit val filePathIsString: FilePath === String = FilePathImpl.filePathInfo.filePathIsString

  implicit class FilePathExtensions(val filePath: FilePath) extends AnyVal {
    private[platform] def pathName: String = filePathIsString.coerce(filePath)
  }

  private[platform] def toFilePath(path: String): FilePath = filePathIsString.flip.coerce(path)


}
