package dev.argon.platform

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node-stream-zip", JSImport.Default)
private[platform] class NodeStreamZip(@unused options: NodeStreamZip.Options) extends js.Object {

  def on(@unused event: "ready", @unused callback: js.Function0[Unit]): Unit = js.native
  def on(@unused event: "error", @unused callback: js.Function1[Any, Unit]): Unit = js.native
  def stream(@unused path: String, @unused callback: js.Function2[Any, NodeReadable, Unit]): Unit = js.native
  def close(): Unit = js.native

}

private[platform] object NodeStreamZip {

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
