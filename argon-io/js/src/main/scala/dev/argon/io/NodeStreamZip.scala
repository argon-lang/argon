package dev.argon.io

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node-stream-zip", JSImport.Default)
class NodeStreamZip(options: NodeStreamZip.Options) extends js.Object {

  def on(event: "ready", callback: js.Function0[Unit]): Unit = js.native
  def on(event: "error", callback: js.Function1[js.Error, Unit]): Unit = js.native
  def stream(path: String, callback: js.Function2[js.Error, NodeReadable, Unit]): Unit = js.native
  def close(): Unit = js.native

}

object NodeStreamZip {

  trait Options extends js.Object {
    val file: String
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  object Options {
    def apply(file: String): Options =
      js.Dynamic.literal(
        file = file,
      ).asInstanceOf[Options]
  }

}
