package dev.argon.io
import zio.stream.ZStream

final class EmptyBinaryResource[R, E] extends BinaryResource[R, E] {
  override def asBytes: ZStream[R, E, Byte] = ZStream.empty
  override def fileName: Option[String] = None
}
