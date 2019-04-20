package dev.argon.util

import java.io.File

object FilenameManip {

  def getExtension(file: File): String = {
    val name = file.getName
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(index + 1)
    else ""
  }

  def getBasename(file: File): String = {
    val name = file.getName
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(0, index)
    else name
  }

}
