package dev.argon.plugin

import dev.argon.plugin.{api => japi}
import zio.*
import scala.reflect.TypeTest
import dev.argon.util.ErrorWrapper
import java.io.IOException
import scala.jdk.CollectionConverters.*

private[plugin] class JavaBackend[E >: IOException, EX <: Exception, Options, Output](impl: japi.Backend[EX, Options, Output])(using TypeTest[Throwable, EX], ErrorWrapper[E, EX]) extends Backend[E, Options, Output] {
  
  def emitModule(options: Options, platforms: Set[Platform[E]])(tube: SerializedTube[E]): IO[E, Output] =
    ZIO.runtime.flatMap { runtime =>
      given Runtime[Any] = runtime

      val javaPlatforms = platforms.map(platform => new JavaPlatformWrap(platform) : japi.Platform[EX, ?, ?, ?]).asJava

      ZIO.attemptBlockingInterrupt {
        impl.emitModule(options, javaPlatforms, new JavaSerializedTubeWrap[E, EX](tube))
      }.catchAll(JavaErrorHandler.handleErrors)
    }
    

}
