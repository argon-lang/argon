package dev.argon.util.graalext

import org.graalvm.polyglot.*
import org.graalvm.polyglot.proxy.ProxyExecutable
import java.nio.charset.{CharsetDecoder, CoderResult, StandardCharsets}
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.Charset

import dev.argon.util.*

object TextDecoderPolyfill {
  def polyfill(context: Context): Unit = {
    val createDecoderState = new ProxyExecutable {
      override def execute(arguments: Value*): AnyRef =
        arguments match {
          case Seq(label, options) => DecoderState(
            decoder = Charset.forName(label.asString()).newDecoder(),
            byteBuffer = ByteBuffer.allocate(4096),
            charBuffer = CharBuffer.allocate(4096),
            fatal = options.getMember("fatal").toOption.map(_.asBoolean()).getOrElse(false),
            ignoreBOM = options.getMember("ignoreBOM").toOption.map(_.asBoolean()).getOrElse(false),
          )

          case _ => throw new RuntimeException("Invalid arguments for createDecoderState")
        }
    }

    val decode = new ProxyExecutable {
      override def execute(arguments: Value*): AnyRef | Null =
        arguments match {
          case Seq(state, buffer, options) =>
            val isStream = options.getMember("stream").toOption.map(_.asBoolean()).getOrElse(false)
            state.as(classOf[DecoderState]).nn.decode(buffer, isStream = isStream)

          case _ => throw new RuntimeException("Invalid arguments for decode")
        }
    }

    context.eval("js",
      """
        |(createDecoderState, decode) => {
        |  if(typeof globalThis.TextDecoder === "undefined") {
        |    globalThis.TextDecoder = class {
        |      #state;
        |
        |      constructor(label = "utf-8", options = {}) {
        |        this.#state = createDecoderState(label, options);
        |        this.encoding = label;
        |        this.fatal = options?.fatal ?? false;
        |        this.ignoreBOM = options?.ignoreBOM ?? false;
        |      }
        |
        |      decode(buffer, options = {}) {
        |        if(buffer === undefined) {
        |          buffer = new Uint8Array();
        |        }
        |        else if(buffer instanceof ArrayBuffer) {
        |          buffer = new Uint8Array(buffer);
        |        }
        |        else if(!(buffer instanceof Uint8Array)) {
        |          if(ArrayBuffer.isView(buffer)) {
        |            buffer = new Uint8Array(buffer.buffer, buffer.byteOffset, buffer.byteLength);
        |          }
        |          else {
        |            throw new TypeError("Invalid buffer");
        |          }
        |        }
        |
        |        return decode(this.#state, buffer, options);
        |      }
        |    };
        |  }
        |}
        |""".stripMargin
    ).execute(createDecoderState, decode)
  }


  private final class DecoderState(
    val decoder: CharsetDecoder,
    val byteBuffer: ByteBuffer,
    val charBuffer: CharBuffer,
    val fatal: Boolean,
    val ignoreBOM: Boolean,
  ) {
    var inProgress: Boolean = false


    def decode(arr: Value, isStream: Boolean): String | Null =
      val sb = new StringBuilder()

      var arrIndex = 0L
      while arrIndex < arr.getArraySize() do
        byteBuffer.clear()

        var n = Math.min(arr.getArraySize() - arrIndex, byteBuffer.remaining());
        while n > 0 do
          byteBuffer.put(arr.getArrayElement(arrIndex).asInt().toByte)
          n -= 1
          arrIndex += 1
        end while
        byteBuffer.flip()

        val eof = !isStream && arrIndex < arr.getArraySize()

        while
          charBuffer.clear()
          val decodeRes = decoder.decode(byteBuffer, charBuffer, eof)
          charBuffer.flip()


          if !inProgress && charBuffer.remaining() > 0 then
            if !ignoreBOM && charBuffer.get(charBuffer.position()) == '\uFEFF' then
              charBuffer.get()
            end if

            inProgress = true
          end if

          sb.append(charBuffer)
        
          if decodeRes.isError() then
            return null
          else
            decodeRes.isOverflow()
        do ()
      end while

      if !isStream then
        inProgress = false
      end if

      sb.toString()
    end decode
  }

}
 

