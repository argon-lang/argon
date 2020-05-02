package dev.argon.platform

import cats.evidence.{===, Is}
import dev.argon.io.Path

private[platform] object FilePathImpl {

  trait FilePathInfo {
    type FilePath
    implicit val pathInstance: Path[FilePath]

    private[platform] implicit val filePathIsString: FilePath === String
  }

  val filePathInfo: FilePathInfo = new FilePathInfo {
    override type FilePath = String
    override implicit val pathInstance: Path[FilePath] = new StringPathInstance()
    override private[platform] val filePathIsString: FilePath === String = Is.refl
  }

}
