package dev.argon.compiler

import cats.data.NonEmptyList
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait ResourceAccess[F[-_, +_, +_], I] {

  type PrintWriter
  type OutputStream
  type InputStream

  def getExtension(id: I): F[Any, NonEmptyList[CompilationError], String]

  def createPrintWriter[A](id: I)(f: PrintWriter => F[Any, NonEmptyList[CompilationError], A]): F[Any, NonEmptyList[CompilationError], A]
  def createOutputStream[A](id: I)(f: OutputStream => F[Any, NonEmptyList[CompilationError], A]): F[Any, NonEmptyList[CompilationError], A]

  def writeText(writer: PrintWriter, text: String): F[Any, NonEmptyList[CompilationError], Unit]

  type ZipWriter
  type ZipReader
  def createZipWriter[A](stream: OutputStream)(f: ZipWriter => F[Any, NonEmptyList[CompilationError], A]): F[Any, NonEmptyList[CompilationError], A]
  def writeZipEntry[A](zip: ZipWriter, path: String)(f: OutputStream => F[Any, NonEmptyList[CompilationError], A]): F[Any, NonEmptyList[CompilationError], A]

  def getZipReader[A](id: I)(f: ZipReader => F[Any, NonEmptyList[CompilationError], A]): F[Any, NonEmptyList[CompilationError], A]
  def getZipEntryInputStream[A](zip: ZipReader, name: String)(f: InputStream => F[Any, NonEmptyList[CompilationError], A]): F[Any, NonEmptyList[CompilationError], A]

  def readProtocolBufferMessage[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(stream: InputStream): F[Any, NonEmptyList[CompilationError], A]
  def writeProtocolBufferMessage(stream: OutputStream, message: GeneratedMessage): F[Any, NonEmptyList[CompilationError], Unit]
}
