package dev.argon.webdemo

import dev.argon.io.{MemoryIOEnvironment, Path}
import zio.{Chunk, Ref, UIO}

object DummyFileSystem {

  val argonCoreFileName: Path = new Path("libraries/Argon.Core.armodule")

  def create: UIO[MemoryIOEnvironment] = for {
    fs <- Ref.make[Map[String, Chunk[Byte]]](Map.empty)
  } yield new MemoryIOEnvironment(fs)



}
