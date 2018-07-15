package com.mi3software.argon.librarygen

import java.io.File

object Program {
  def main(args: Array[String]): Unit =
    new File(".").listFiles
      .filter { _.isDirectory }
      .foreach { dir =>
        println(s"Creating library ${dir.getName}")
        val path = new File(dir, "create.sc").getPath
        ammonite.Main.main(Array(path, path))
      }

}
