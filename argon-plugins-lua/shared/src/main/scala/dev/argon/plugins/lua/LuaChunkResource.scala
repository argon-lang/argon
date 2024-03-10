package dev.argon.plugins.lua

import dev.argon.io.TextResource
import zio.ZIO
import zio.stream.ZStream

import java.nio.charset.CharacterCodingException

trait LuaChunkResource[E] extends TextResource[E] {
  def luaChunk: ZIO[Any, E, AST.Chunk]
}

object LuaChunkResource {
  trait Impl[E >: CharacterCodingException] extends LuaChunkResource[E] with TextResource.Impl[E] {
    override def asText: ZStream[Any, E, String] =
      ZStream.fromZIO(luaChunk).flatMap(_.toStream)
  }
}
