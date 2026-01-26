package dev.argon.backend.scalaApi

import dev.argon.backend.sjs
import dev.argon.util.async.AsyncIterableTools
import dev.argon.util.async.AsyncIterableTools.AsyncIterable
import dev.argon.io.{chunkToUint8Array, uint8ArrayToChunk}
import nobleidl.core.{ErrorType, JSAdapter}
import zio.*
import zio.stream.*
import dev.argon.io

import scala.reflect.TypeTest
import scala.scalajs.js
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.typedarray.Uint8Array

trait BinaryResource[E] {
  val fileName: Option[String]
  def asBytes: Stream[E, Byte]

  def toIOResource: io.BinaryResource[E] =
    new io.BinaryResource[E] {
      override def fileName: Option[String] =
        BinaryResource.this.fileName
        
      override def asBytes: Stream[E, Byte] =
        BinaryResource.this.asBytes
    }
}

object BinaryResource {
  def jsAdapter[SE, JE](eAdapter: JSAdapter[SE, JE])(using Runtime[Any], ErrorType[SE], ErrorType[JE]): JSAdapter[BinaryResource[SE], sjs.BinaryResource[JE]] =
    new JSAdapter[BinaryResource[SE], sjs.BinaryResource[JE]] {
      override def toJS(s: BinaryResource[SE]): sjs.BinaryResource[JE] =
        new sjs.BinaryResource[JE] {
          override val fileName: js.UndefOr[String] = s.fileName.getOrElse(())
          override def asBytes(): AsyncIterable[Uint8Array] =
            AsyncIterableTools.zstreamToAsyncIterableRaw(
              s.asBytes
                .mapError(e => JavaScriptException(eAdapter.toJS(e)))
                .chunks
                .map(chunkToUint8Array)
            )
        }


      override def fromJS(j: sjs.BinaryResource[JE]): BinaryResource[SE] =
        new BinaryResource[SE] {
          override val fileName: Option[String] =
            j.fileName.toOption

          override def asBytes: Stream[SE, Byte] =
            AsyncIterableTools.asyncIterableToZStreamRaw(j.asBytes())
              .catchAll { ex =>
                summon[ErrorType[JE]].checkThrowable(ex) match {
                  case Some(e) => ZStream.fail(eAdapter.fromJS(e))
                  case None => ZStream.die(ex)
                }
              }
              .map(uint8ArrayToChunk)
              .flattenChunks
        }
    }
}

