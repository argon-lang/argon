package dev.argon.compiler

import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait ResourceAccess[F[_], I] {

  type PrintWriter
  type OutputStream
  type InputStream

  def getExtension(id: I): F[String]

  def createPrintWriter[A](id: I)(f: PrintWriter => F[A]): F[A]
  def createOutputStream[A](id: I)(f: OutputStream => F[A]): F[A]

  def writeText(writer: PrintWriter, text: String): F[Unit]

  type ZipWriter
  type ZipReader
  def createZipWriter[A](stream: OutputStream)(f: ZipWriter => F[A]): F[A]
  def writeZipEntry[A](zip: ZipWriter, path: String)(f: OutputStream => F[A]): F[A]

  def getZipReader[A](id: I)(f: ZipReader => F[A]): F[A]
  def getZipEntryInputStream[A](zip: ZipReader, name: String)(f: InputStream => F[A]): F[A]

  def readProtocolBufferMessage[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(stream: InputStream): F[A]
  def writeProtocolBufferMessage(stream: OutputStream, message: GeneratedMessage): F[Unit]
}
