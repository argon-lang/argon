package dev.argon.vm.engine

import zio.*
import typings.argonLangVmEngine.mod.StandardLibrary

import scala.collection.mutable

class TestStandardLibrary(output: mutable.StringBuilder) extends StandardLibrary {
  def getOutput: UIO[String] = ZIO.succeed { output.toString }

  override def print(value: String): Unit = {
    output.append(value)
  }

  def toNativeFunctions: NativeFunctions = nativeFunctions

}

object TestStandardLibrary {
  def make: UIO[TestStandardLibrary] =
    for
      output <- ZIO.succeed { new StringBuilder() }
    yield TestStandardLibrary(output)
}

