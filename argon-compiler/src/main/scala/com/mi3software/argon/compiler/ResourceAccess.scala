package com.mi3software.argon.compiler

import java.io._
import java.util.zip.{ZipEntry, ZipOutputStream}

import com.twitter.scrooge.{ThriftStruct, ThriftStructCodec}
import scalapb.GeneratedMessage

trait ResourceAccess[F[_], I] {
  def getExtension(id: I): F[String]

  def loadThriftStruct[T <: ThriftStruct](id: I)(codec: ThriftStructCodec[T]): F[T]

  def createPrintWriter[A](id: I)(f: PrintWriter => A): F[A]

  def createOutputStream[A](id: I)(f: OutputStream => F[A]): F[A]
  def createZipOutputStream[A](stream: OutputStream)(f: ZipOutputStream => F[A]): F[A]
  def createZipEntry[A](zip: ZipOutputStream, path: String)(f: ZipEntry => F[A]): F[A]
  def writeProtocolBufferMessage(stream: OutputStream, message: GeneratedMessage): F[Unit]
}
