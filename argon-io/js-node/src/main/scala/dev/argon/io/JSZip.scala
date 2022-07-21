package dev.argon.io

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.{Promise, |}
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("jszip", JSImport.Default)
private[io] class JSZip() extends js.Object {

  def file(@unused path: String, @unused data: Uint8Array): this.type = js.native
  def file(@unused path: String): JSZip.JSZipObject = js.native

  def loadAsync(@unused data: Uint8Array): Promise[JSZip] = js.native
  def generateAsync(@unused options: JSZip.JSZipGeneratorOptions): Promise[Uint8Array] = js.native

}

private[io] object JSZip {

  @js.native
  trait JSZipObject extends js.Object {
    val name: String
    val dir: Boolean
    val date: js.Date
    val comment: String
    val unixPermissions: Double | String
    val dosPermissions: Double | Null
    def async(t: "uint8array"): Promise[Uint8Array]
  }

  @js.native
  trait JSZipGeneratorOptions extends js.Object {
    val `type`: "uint8array"
  }

}
