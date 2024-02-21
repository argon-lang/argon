package dev.argon.plugins.lua

import dev.argon.io.TextResource
import zio.ZIO
import zio.stream.ZStream

import java.nio.charset.CharacterCodingException

trait LuaChunkResource[R, E] extends TextResource[R, E] {
  def luaChunk: ZIO[R, E, AST.Chunk]
}

object LuaChunkResource {
  trait Impl[R, E >: CharacterCodingException] extends LuaChunkResource[R, E] with TextResource.Impl[R, E] {
    override def asText: ZStream[R, E, String] =
      ZStream.fromZIO(luaChunk).flatMap(_.toStream)
  }
}
