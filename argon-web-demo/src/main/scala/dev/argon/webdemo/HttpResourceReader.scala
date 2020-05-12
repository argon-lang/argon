package dev.argon.webdemo

import java.io.{FileNotFoundException, IOException}
import java.nio.charset.StandardCharsets

import cats.implicits._
import cats.data.NonEmptyList
import dev.argon.backend.ResourceReader
import dev.argon.compiler.{Comp, CompilationError, CompilationMessageSource, ErrorList}
import dev.argon.io.ZipFileReader
import dev.argon.io.fileio.FileIOLite
import dev.argon.platform.ResourceReaderMemZipBase
import dev.argon.stream.builder.Source
import org.scalajs.dom.raw.XMLHttpRequest
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.stream._
import zio._

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}


object HttpResourceReader {

  def live: ZLayer[FileIOLite, Nothing, ResourceReader[WebDemoResourceIndicator]] =
    ZLayer.fromFunction { env =>
      new ResourceReader.Service[WebDemoResourceIndicator] with ResourceReaderMemZipBase {
        protected def ioExceptionToError(ex: IOException): NonEmptyList[CompilationError] =
          NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

        @SuppressWarnings(Array("dev.argon.warts.ZioEffect", "org.wartremover.warts.AsInstanceOf"))
        private def readHttp(url: String): Stream[ErrorList, Chunk[Byte]] =
          ZStream.fromEffect(
            ZIO.effectAsync { complete =>
              val request = new XMLHttpRequest()
              request.responseType = "arraybuffer"
              request.onload = { _ =>
                if(request.status === 200) {
                  val arrBuffer = request.response.asInstanceOf[ArrayBuffer]
                  complete(IO.succeed(Chunk.fromArray(new Int8Array(arrBuffer).toArray)))
                }
                else {
                  complete(IO.fail(ioExceptionToError(new FileNotFoundException)))
                }
              }
              request.onerror = { e =>
                complete(IO.fail(ioExceptionToError(new IOException(e.message))))
              }

              request.open("GET", url)
            }
          )

        private def readResource(id: WebDemoResourceIndicator): Stream[ErrorList, Chunk[Byte]] =
          id match {
            case UriResourceIndicator(uri) =>
              readHttp(uri)

            case LocalResourceIndicator(_, content) =>
              Stream(Chunk.fromArray(content.getBytes(StandardCharsets.UTF_8)))
          }

        override type ZipReader = ZipFileReader[Any, ErrorList]

        override def getZipReader(id: WebDemoResourceIndicator): Managed[ErrorList, ZipFileReader[Any, ErrorList]] =
          ZManaged.fromEffect(zipReaderForStream(ioExceptionToError)(readResource(id)))

        override def zipEntryStream(zip: ZipFileReader[Any, ErrorList], name: String): Source[Any, ErrorList, Chunk[Byte], Unit] =
          zip.getEntryStream(name)

        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Any, ErrorList, Chunk[Byte], Unit]): Comp[A] =
          env.get.deserializeProtocolBuffer(ioExceptionToError)(companion)(data)
      }
    }

}
