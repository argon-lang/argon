package dev.argon.backend.js

import java.nio.charset.StandardCharsets

import org.apache.commons.io.IOUtils
import org.apache.commons.io.input.NullInputStream
import org.apache.commons.io.output.NullOutputStream
import org.graalvm.polyglot.{Context, Source}
import zio.blocking.Blocking
import zio.{IO, RIO, Task, ZIO}

import scala.jdk.CollectionConverters._

object JSModuleExtractor {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def exportedFunctions(module: String): RIO[Blocking, Map[String, String]] =
    ZIO.accessM[Blocking](_.get.effectBlocking {
      val context = Context.newBuilder("js")
        .out(new NullOutputStream)
        .err(new NullOutputStream)
        .in(new NullInputStream(0))
        .build()
      try {
        val extractorCode = {
          val stream = getClass.getClassLoader.getResourceAsStream("js-module-extractor.js")
          try IOUtils.toString(stream, StandardCharsets.UTF_8)
          finally stream.close()
        }

        val mainSource = Source.newBuilder("js",

          extractorCode,
          "js-module-extractor.js"
        )
          .mimeType("application/javascript")
          .build()

        val _ = context.eval(mainSource)
        val extractModuleFunctions = context.eval("js", "ModuleExtractor.extractModuleFunctions")

        val exportedFunctions = extractModuleFunctions.execute(module)

        exportedFunctions.getMemberKeys
          .asScala
          .toSeq
          .map { name => name -> exportedFunctions.getMember(name).asString() }
          .toMap
      }
      finally context.close()
    })

}
