package dev.argon.plugin

import dev.argon.compiler.tube.TubeName
import zio.*
import java.io.IOException
import dev.argon.plugin.{api => japi}
import scala.jdk.CollectionConverters.*
import dev.argon.util.{*, given}

final class JavaBuildOutputExecutors[Output](inner: japi.OutputExecutor[Output]) extends BuildOutputExecutor[Output] {
  override def execute(libraries: Map[TubeName, Output], buildOutput: Output): IO[IOException, (ExitCode, String)] =
    ZIO.attemptBlockingInterrupt {
      val javaLibs = libraries
        .map { case (name, output) => name.name.toList.asJava -> output }
        .asJava
      
      val result = inner.execute(javaLibs, buildOutput)
      (ExitCode(result.exitCode), result.output)
    }.refineToOrDie[IOException]

}
