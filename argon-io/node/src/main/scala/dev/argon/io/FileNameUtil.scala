package dev.argon.io

object FileNameUtil {
  def getBaseName(fileName: String): String = NodePath.basename(fileName)

  def getBaseNameWithoutExtension(fileName: String): String =
    NodePath.basename(fileName, NodePath.extname(fileName))

  def getParentDirectory(fileName: String): Option[String] =
    Some(NodePath.parse(fileName).dir).filterNot(_.isEmpty)

  def getExtension(fileName: String): String = NodePath.extname(fileName).stripPrefix(".")

  def combine(a: String, b: String): String = NodePath.join(a, b)
}

