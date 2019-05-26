package dev.argon.compiler

import cats.data.NonEmptyList
import dev.argon.util.stream.{ArStream, Resource, StreamTransformation, ZipEntryInfo}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait ResourceAccess[F[-_, +_, +_], R, I] {

  type InputStream

  def getExtension(id: I): F[R, NonEmptyList[CompilationError], String]

  def resourceSink(id: I): Resource[F, R, NonEmptyList[CompilationError], StreamTransformation[F, R, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit]]
  def zipFromEntries(entryStream: ArStream[F, R, NonEmptyList[CompilationError], ZipEntryInfo[F, R, NonEmptyList[CompilationError]]]): ArStream[F, R, NonEmptyList[CompilationError], Byte]

  type ZipReader
  def getZipReader[A](id: I)(f: ZipReader => F[R, NonEmptyList[CompilationError], A]): F[R, NonEmptyList[CompilationError], A]
  def getZipEntryInputStream[A](zip: ZipReader, name: String)(f: InputStream => F[R, NonEmptyList[CompilationError], A]): F[R, NonEmptyList[CompilationError], A]

  def readProtocolBufferMessage[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(stream: InputStream): F[R, NonEmptyList[CompilationError], A]
  def protocolBufferStream(message: GeneratedMessage): ArStream[F, R, NonEmptyList[CompilationError], Byte]
}
