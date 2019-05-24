package dev.argon.compiler

import cats.data.NonEmptyList
import dev.argon.util.stream.{ArStream, Resource, StreamTransformation, ZipEntryInfo}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait ResourceAccess[F[-_, +_, +_], I] {

  type InputStream

  def getExtension(id: I): F[Any, NonEmptyList[CompilationError], String]

  def resourceSink(id: I): Resource[F, Any, NonEmptyList[CompilationError], StreamTransformation[F, Any, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit]]
  def zipFromEntries(entryStream: ArStream[F, Any, NonEmptyList[CompilationError], ZipEntryInfo[F, Any, NonEmptyList[CompilationError]]]): ArStream[F, Any, NonEmptyList[CompilationError], Byte]

  type ZipReader
  def getZipReader[A](id: I)(f: ZipReader => F[Any, NonEmptyList[CompilationError], A]): F[Any, NonEmptyList[CompilationError], A]
  def getZipEntryInputStream[A](zip: ZipReader, name: String)(f: InputStream => F[Any, NonEmptyList[CompilationError], A]): F[Any, NonEmptyList[CompilationError], A]

  def readProtocolBufferMessage[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(stream: InputStream): F[Any, NonEmptyList[CompilationError], A]
  def protocolBufferStream(message: GeneratedMessage): ArStream[F, Any, NonEmptyList[CompilationError], Byte]
}
