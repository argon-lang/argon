package dev.argon.webdemo

import java.io.{FileNotFoundException, IOException}
import java.nio.charset.StandardCharsets

import cats.implicits._
import cats.data.NonEmptyList
import dev.argon.compiler.loaders.ResourceReader
import dev.argon.compiler.{Comp, CompilationError, Compilation, DiagnosticSource}
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
        protected def ioExceptionToError(ex: IOException): CompilationError =
          Compilation.errorForIOException(ex)

        @SuppressWarnings(Array("dev.argon.warts.ZioEffect", "org.wartremover.warts.AsInstanceOf"))
        private def readHttp(url: String): Stream[CompilationError, Byte] =
          ZStream.unwrap(
            ZIO.effectAsync { complete =>
              val request = new XMLHttpRequest()
              request.responseType = "arraybuffer"
              request.onload = { _ =>
                if(request.status === 200) {
                  val arrBuffer = request.response.asInstanceOf[ArrayBuffer]
                  complete(IO.succeed(ZStream.fromChunk(Chunk.fromArray(new Int8Array(arrBuffer).toArray))))
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

        @SuppressWarnings(Array("dev.argon.warts.ZioEffect", "org.wartremover.warts.AsInstanceOf"))
        private def readHttpStr(url: String): IO[CompilationError, String] =
          ZIO.effectAsync { complete =>
            val request = new XMLHttpRequest()
            request.responseType = "text"
            request.onload = { _ =>
              if(request.status === 200) {
                val str = request.response.asInstanceOf[String]
                complete(IO.succeed(str))
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

        private def readResource(id: WebDemoResourceIndicator): Stream[CompilationError, Byte] =
          id match {
            case UriResourceIndicator(uri) =>
              readHttp(uri)

            case LocalResourceIndicator(_, content) =>
              Stream.fromChunk(Chunk.fromArray(content.getBytes(StandardCharsets.UTF_8)))
          }


        override def readFile(id: WebDemoResourceIndicator): Stream[CompilationError, Byte] =
          id match {
            case UriResourceIndicator(uri) => readHttp(uri)
            case LocalResourceIndicator(_, content) => ZStream.fromChunk(Chunk.fromArray(content.getBytes(StandardCharsets.UTF_8)))
          }

        override def readTextFile(id: WebDemoResourceIndicator): Stream[CompilationError, Char] =
          ZStream.unwrap(readTextFileAsString(id).map { str =>
            ZStream.fromChunk(Chunk.fromArray(str.toCharArray))
          })

        override def readTextFileAsString(id: WebDemoResourceIndicator): Comp[String] =
          id match {
            case UriResourceIndicator(uri) => readHttpStr(uri)
            case LocalResourceIndicator(_, content) => IO.succeed(content)
          }

        override def getZipReader(id: WebDemoResourceIndicator): Managed[CompilationError, ZipFileReader[Any, CompilationError]] =
          ZManaged.fromEffect(zipReaderForStream(ioExceptionToError)(readResource(id)))

        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Stream[CompilationError, Byte]): Comp[A] =
          env.get.deserializeProtocolBuffer(ioExceptionToError)(companion)(data)
      }
    }

}
