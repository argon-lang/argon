package dev.argon.io

import java.nio.file.Path

import org.apache.commons.io.FilenameUtils


object FileNameUtil {
  def getBaseName(fileName: String): String = FilenameUtils.getName(fileName)

  def getBaseNameWithoutExtension(fileName: String): String =
    FilenameUtils.getBaseName(fileName)

  @SuppressWarnings(Array("scalafix:Disable.toString"))
  def getParentDirectory(fileName: String): Option[String] =
    Option(Path.of(fileName).getParent).map(_.toString)

  def getExtension(fileName: String): String = FilenameUtils.getExtension(fileName)

  @SuppressWarnings(Array("scalafix:Disable.toString"))
  def combine(a: String, b: String): String = java.nio.file.Path.of(a, b).toString
}
