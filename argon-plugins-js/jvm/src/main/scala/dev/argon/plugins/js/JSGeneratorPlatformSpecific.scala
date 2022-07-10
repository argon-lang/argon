package dev.argon.plugins.js

import org.apache.commons.io.IOUtils
import zio.*
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.Source
import org.graalvm.polyglot.io.FileSystem

import java.nio.charset.StandardCharsets


trait JSGeneratorPlatformSpecific {
  protected def generateImpl(value: JSValue): UIO[String] =
    ZIO.attemptBlockingInterrupt {
      val context = Context.newBuilder("js")
        .allowIO(true)
        .fileSystem(new ResourceFileSystem)
        .option("engine.WarnInterpreterOnly", false.toString)
        .build()

      val source = Source
        .newBuilder("js", astringGenerateScript, "astring_generate.mjs")
        .mimeType("application/javascript+module")
        .build()

      val generate = context.eval(source)

      generate.execute(value).asString()
    }.orDie


  private val astringGenerateScript =
    """
      | import {generate} from "astring.mjs";
      | generate;
      |""".stripMargin
}



