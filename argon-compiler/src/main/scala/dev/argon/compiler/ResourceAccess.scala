package dev.argon.compiler

import cats.data.NonEmptyList
import dev.argon.stream.{ArStream, Resource, StreamTransformation, ZipEntryInfo}
import dev.argon.stream.{Resource, StreamTransformation, ZipEntryInfo}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait ResourceAccess[F[-_, +_, +_], R, I] {

  def getExtension(id: I): F[R, NonEmptyList[CompilationError], String]

  def resourceSink(id: I): Resource[F, R, NonEmptyList[CompilationError], StreamTransformation[F, R, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit]]
  def zipFromEntries(entryStream: ArStream[F, R, NonEmptyList[CompilationError], ZipEntryInfo[F, R, NonEmptyList[CompilationError]]]): ArStream[F, R, NonEmptyList[CompilationError], Byte]

  type ZipReader
  def getZipReader[A](id: I): Resource[F, R, NonEmptyList[CompilationError], ZipReader]
  def zipEntryStream(zip: ZipReader, name: String): ArStream[F, R, NonEmptyList[CompilationError], Byte]

  def protocolBufferSink[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A]): StreamTransformation[F, R, NonEmptyList[CompilationError], Byte, Unit, Nothing, A]
  def protocolBufferStream(message: GeneratedMessage): ArStream[F, R, NonEmptyList[CompilationError], Byte]
}
