package dev.argon.vm.engine

import dev.argon.argonvm.StandardLibrary
import zio.*

class TestStandardLibrary(output: StringBuffer) extends StandardLibrary {
  def getOutput: UIO[String] = ZIO.succeed { output.toString }

  override def print(value: String): Unit = {
    output.append(value)
  }
}

object TestStandardLibrary {
  def make: UIO[TestStandardLibrary] =
    for
      output <- ZIO.succeed { new StringBuffer() }
    yield TestStandardLibrary(output)
}

