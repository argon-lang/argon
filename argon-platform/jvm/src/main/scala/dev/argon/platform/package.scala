package dev.argon

import cats.evidence.===
import dev.argon.io.Path
import java.nio.file.{Path => JPath}
import zio._

package object platform {
  type FilePath = FilePathImpl.filePathInfo.FilePath
  implicit val filePathPathInstance: Path[FilePath] = FilePathImpl.filePathInfo.pathInstance
  private[platform] implicit val filePathIsJavaPath: FilePath === JPath = FilePathImpl.filePathInfo.filePathIsJavaPath

  implicit class FilePathExtensions(val filePath: FilePath) extends AnyVal {
    private[platform] def javaPath: JPath = filePathIsJavaPath.coerce(filePath)
  }

  implicit class JPathExtensions(val path: JPath) extends AnyVal {
    private[platform] def toFilePath: FilePath = filePathIsJavaPath.flip.coerce(path)
  }


}
