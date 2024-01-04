package dev.argon.io.jstypes.jszip

import dev.argon.io.jstypes.jszip.JSZip.InputByType
import org.scalajs.dom.Blob

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

@js.native
@JSImport("jszip", JSImport.Default)
class JSZip() extends js.Object {

  def file(name: String): JSZip | Null = js.native
  def file(name: js.RegExp): js.Array[JSZip.JSZipObject] = js.native
  def file[T <: JSZip.InputType](path: String, data: JSZip.InputByType[T] | js.Promise[JSZip.InputByType[T]], options: JSZip.JSZipFileOptions = js.native): JSZip = js.native

  def folder(name: String): JSZip | Null = js.native
  def folder(name: js.RegExp): js.Array[JSZip.JSZipObject] = js.native

  def forEach(callback: js.Function2[String, JSZip.JSZipObject, Unit]): Unit = js.native

  def generateAsync[T <: JSZip.OutputType](options: JSZip.JSZipGeneratorOptions[T]): js.Promise[JSZip.OutputByType[T]] = js.native
  def generateAsync[T <: JSZip.OutputType](options: JSZip.JSZipGeneratorOptions[T], onUpdate: JSZip.OnUpdateCallback = js.native): js.Promise[JSZip.OutputByType[T]] = js.native
  def loadAsync(data: JSZip.InputFileFormat, options: JSZip.JSZipLoadOptions = js.native): js.Promise[JSZip] = js.native

}

object JSZip {
  type InputType = "base64" | "string" | "text" | "binarystring" | "array" | "uint8array" | "arraybuffer" | "blob" | "stream"
  type OutputType = "base64" | "string" | "text" | "binarystring" | "array" | "uint8array" | "arraybuffer" | "blob" | "nodebuffer"

  trait JSZipMetadata extends js.Object {
    val percent: Double
    val currentFile: String | Null
  }

  type OnUpdateCallback = js.Function1[JSZipMetadata, Unit]

  type Compression = "STORE" | "DEFLATE"

  trait CompressionOptions extends js.Object {
    val level: Double
  }

  type InputByType[T <: InputType] =
    T match {
      case "base64" | "string" | "text" | "binarystring" => String
      case "array" => js.Array[Double]
      case "uint8array" => Uint8Array
      case "arraybuffer" => ArrayBuffer
      case "blob" => Blob
      case "stream" => Nothing
    }

  type OutputByType[T <: OutputType] =
    T match {
      case "base64" | "string" | "text" | "binarystring" => String
      case "array" => js.Array[Double]
      case "uint8array" => Uint8Array
      case "arraybuffer" => ArrayBuffer
      case "blob" => Blob
      case "nodebuffer" => Any
    }

  type InputFileFormatTypes = String | js.Array[Double] | Uint8Array | ArrayBuffer | Blob
  type InputFileFormat = InputFileFormatTypes | js.Promise[InputFileFormatTypes]

  trait JSZipObject extends js.Object {
    val name: String
    val unsafeOriginalName: js.UndefOr[String]
    val dir: Boolean
    val date: js.Date
    val comment: String
    val unixPermissions: Double | String | Null
    val dosPermissions: Double | Null
    val options: JSZipObjectOptions

    def async[T <: OutputType](`type`: T): js.Promise[OutputByType[T]]
    def async[T <: OutputType](`type`: T, onUpdate: OnUpdateCallback): js.Promise[OutputByType[T]]
  }

  trait JSZipFileOptions extends js.Object {
    val base64: js.UndefOr[Boolean] = js.undefined
    val binary: js.UndefOr[Boolean] = js.undefined
    val date: js.UndefOr[js.Date] = js.undefined
    val compression: js.UndefOr[Compression] = js.undefined
    val compressionOptions: js.UndefOr[CompressionOptions | Null] = js.undefined
    val comment: js.UndefOr[String] = js.undefined
    val optimizedBinaryString: js.UndefOr[Boolean] = js.undefined
    val createFolders: js.UndefOr[Boolean] = js.undefined
    val dir: js.UndefOr[Boolean] = js.undefined
    val dosPermissions: js.UndefOr[Double | Null] = js.undefined
    val unixPermissions: js.UndefOr[Double | String | Null] = js.undefined
  }

  trait JSZipObjectOptions extends js.Object {
    val compression: Compression
  }

  trait JSZipGeneratorOptions[T <: OutputType] extends js.Object {
    val compression: js.UndefOr[Compression] = js.undefined
    val compressionOptions: js.UndefOr[CompressionOptions | Null] = js.undefined
    val `type`: js.UndefOr[T] = js.undefined
    val comment: js.UndefOr[String] = js.undefined
    val mimeType: js.UndefOr[String] = js.undefined
    val encodeFileName: js.UndefOr[js.Function1[String, String]] = js.undefined
    val streamFiles: js.UndefOr[Boolean] = js.undefined
    val platform: js.UndefOr["DOS" | "UNIX"] = js.undefined
  }

  trait JSZipLoadOptions extends js.Object {
    val base64: js.UndefOr[Boolean] = js.undefined
    val checkCRC32: js.UndefOr[Boolean] = js.undefined
    val optimizedBinaryString: js.UndefOr[Boolean] = js.undefined
    val createFolders: js.UndefOr[Boolean] = js.undefined
    val decodeFileName: js.UndefOr[js.Function1[js.Array[String] | Uint8Array, String]] = js.undefined
  }


}
