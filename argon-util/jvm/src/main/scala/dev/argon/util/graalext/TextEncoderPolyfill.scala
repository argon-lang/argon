package dev.argon.util.graalext

import org.graalvm.polyglot.{Context, Value}
import org.graalvm.polyglot.proxy.ProxyExecutable

import java.nio.charset.StandardCharsets

object TextEncoderPolyfill {


  def polyfill(context: Context): Unit =
    val encodeFunc = new ProxyExecutable {
      override def execute(arguments: Value*): AnyRef =
        val s = arguments(0).asString()

        val b = s.getBytes(StandardCharsets.UTF_8)
        val arr = context.eval("js", "n => new Uint8Array(n)").execute(b.length);
        for i <- b.indices do
          arr.setArrayElement(i, b(i))

        arr
      end execute
    }

    context.eval("js",
      """
        |(encode) => {
        |  if(typeof globalThis.TextEncoder === "undefined") {
        |    globalThis.TextEncoder = class {};
        |    globalThis.TextEncoder.prototype.encode = encode;
        |    globalThis.TextEncoder.prototype.encoding = "utf-8";
        |  }
        |}
        |""".stripMargin
    ).execute(encodeFunc)

  end polyfill

}
