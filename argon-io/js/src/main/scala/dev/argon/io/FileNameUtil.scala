package dev.argon.io

object FileNameUtil {
  def getBaseName(fileName: String): String = {
    val slash = fileName.lastIndexOf('/')
    if(slash >= 0) fileName.substring(slash + 1)
    else fileName
  }

  def getBaseNameWithoutExtension(fileName: String): String = {
    val lastSlash = fileName.lastIndexOf('/')
    val dot = fileName.lastIndexOf('.')

    if(lastSlash >= 0) {
      if(dot >= lastSlash + 1) fileName.substring(lastSlash + 1, dot)
      else fileName.substring(lastSlash + 1)
    }
    else {
      if(dot > 0) fileName.substring(0, dot)
      else fileName
    }
  }

  def getExtension(fileName: String): String = {
    val lastSlash = fileName.lastIndexOf('/')
    val dot = fileName.lastIndexOf('.')

    val useDot =
      if(lastSlash >= 0) dot > lastSlash + 1
      else dot > 0

    if(useDot) fileName.substring(dot + 1)
    else ""
  }

  def getParentDirectory(fileName: String): Option[String] = {
    val startIndex = if(fileName.endsWith("/")) fileName.length - 2 else fileName.length - 1

    val slash = fileName.lastIndexOf('/', startIndex)

    if(slash >= 0) Some(fileName.substring(0, slash))
    else None
  }

  def combine(a: String, b: String): String = {
    if(b.startsWith("/")) b
    else a + "/" + b
  }
}
