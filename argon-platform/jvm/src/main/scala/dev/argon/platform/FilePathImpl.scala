package dev.argon.platform

import cats.evidence.{===, Is}
import dev.argon.io.Path
import java.nio.file.{Path => JPath}

private[platform] object FilePathImpl {

  trait FilePathInfo {
    type FilePath
    implicit val pathInstance: Path[FilePath]

    private[platform] implicit val filePathIsJavaPath: FilePath === JPath
  }

  val filePathInfo: FilePathInfo = new FilePathInfo {
    override type FilePath = JPath
    override implicit val pathInstance: Path[FilePath] = new JavaPathInstance
    override private[platform] val filePathIsJavaPath: FilePath === JPath = Is.refl
  }

}
