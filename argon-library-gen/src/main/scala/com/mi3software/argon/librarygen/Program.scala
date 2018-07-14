package com.mi3software.argon.librarygen

import java.io.File

object Program {
  def main(args: Array[String]): Unit =
    new File(".").listFiles
      .filter { _.isDirectory }
      .foreach { dir =>
        println(s"Creating library ${dir.getName}")
        ammonite.Main.main(Array(dir.getPath + "/create.sc"))
      }

}
