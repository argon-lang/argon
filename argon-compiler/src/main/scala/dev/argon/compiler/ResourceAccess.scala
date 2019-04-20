package dev.argon.compiler

import java.io._
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait ResourceAccess[F[_], I] {
  def getExtension(id: I): F[String]

  def createPrintWriter[A](id: I)(f: PrintWriter => A): F[A]

  def createOutputStream[A](id: I)(f: OutputStream => F[A]): F[A]
  def createZipOutputStream[A](stream: OutputStream)(f: ZipOutputStream => F[A]): F[A]
  def createZipEntry[A](zip: ZipOutputStream, path: String)(f: ZipEntry => F[A]): F[A]

  type ZipReader
  def getZipFile[A](id: I)(f: ZipReader => F[A]): F[A]
  def getZipEntryStream[A](zip: ZipReader, name: String)(f: InputStream => F[A]): F[A]

  def readProtocolBufferMessage[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(stream: InputStream): F[A]
  def writeProtocolBufferMessage(stream: OutputStream, message: GeneratedMessage): F[Unit]
}
