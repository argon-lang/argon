package dev.argon.util

import com.google.protobuf.{CodedInputStream, CodedOutputStream}

trait StreamableMessage[A >: Null] {
  def writeElement(value: A)(output: CodedOutputStream): Unit
  def readElement(input: CodedInputStream): A
}
