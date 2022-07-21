package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.charset.CharacterCodingException

trait Resource[-R, +E] {
  def fileName: Option[String]
}

object Resource:
  trait WithoutFileName:
    def fileName: Option[String] = None
  end WithoutFileName
end Resource


