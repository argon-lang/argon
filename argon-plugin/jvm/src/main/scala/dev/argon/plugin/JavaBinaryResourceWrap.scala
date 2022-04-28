package dev.argon.plugin


import dev.argon.plugin.{api => japi}
import dev.argon.io.BinaryResource
import java.nio.channels.SeekableByteChannel
import java.io.InputStream
import zio.*
import dev.argon.util.{*, given}
import java.io.IOException

final class JavaBinaryResourceWrap[E >: IOException, EX <: Exception](resource: BinaryResource[E])(using Runtime[Any], ErrorWrapper[E, EX]) extends japi.resource.BinaryResource[EX] {
  override def asInputStream(): InputStream =
    JavaExecuteIO.runInterruptable(resource.asInputStream)
      .getOrElse { JavaInputStreamWrap.fromZStream(resource.asBytes) }
    

  override def asSeekableByteChannel(): SeekableByteChannel =
    JavaExecuteIO.runInterruptable(resource.asSeekableByteChannel).orNull
}
