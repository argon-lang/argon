package com.mi3software.argon.compiler

import java.io.PrintWriter

import com.twitter.scrooge.{ThriftStruct, ThriftStructCodec}

trait ResourceAccess[F[_], I] {
  def getExtension(id: I): F[String]

  def loadThriftStruct[T <: ThriftStruct](id: I)(codec: ThriftStructCodec[T]): F[T]
}
