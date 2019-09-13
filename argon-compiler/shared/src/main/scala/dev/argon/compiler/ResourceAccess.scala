package dev.argon.compiler

import cats.data.NonEmptyList
import dev.argon.compiler.core._
import dev.argon.io.ZipEntryInfo
import dev.argon.stream.builder.Source
import dev.argon.stream.Resource
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio.Chunk

trait ResourceAccess[TContext <: Context with Singleton] {

  val context: TContext

  import context._

  def getExtension(id: ResIndicator): Comp[String]

  def writeToResource[X](id: ResIndicator)(data: Source[Comp, Chunk[Byte], X]): Comp[X]
  def zipFromEntries(entries: Source[Comp, ZipEntryInfo[Comp], Unit]): Source[Comp, Chunk[Byte], Unit]

  type ZipReader
  def getZipReader[A](id: ResIndicator): Resource[Comp, ZipReader]
  def zipEntryStream(zip: ZipReader, name: String): Source[Comp, Chunk[Byte], Unit]

  def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage with Message[A]]
  (companion: GeneratedMessageCompanion[A])
  (data: Source[Comp, Chunk[Byte], Unit])
  : Comp[A]
  def serializeProtocolBuffer(message: GeneratedMessage): Source[Comp, Chunk[Byte], Unit]
}

trait ResourceAccessFactory[-TContext <: Context] {

  def create(context: TContext): ResourceAccess[context.type]

}
