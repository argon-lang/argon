package dev.argon.plugins.js

import dev.argon.util.{*, given}

import scala.jdk.CollectionConverters.given
import org.graalvm.polyglot.proxy.{ProxyArray, ProxyObject}
import com.oracle.truffle.js.runtime.BigInt as JSBigInt
import org.graalvm.polyglot.{Context, Source, Value}

import java.util.{List as JList, Map as JMap}
import zio.*

import scala.util.control.NonFatal

object JSValueUtil:
  type JSValue = Value

  final case class JSRegExp(source: String, flags: String)

  trait JSContextPlatformSpecific {
    def make: URIO[Scope, JSContext] =
      ZIO.fromAutoCloseable(ZIO.succeed { createContext() })
        .map(JSContextImpl(_))

    private def createContext(): Context =
      Context.newBuilder("js")
        .allowIO(true)
        .fileSystem(new ResourceFileSystem)
        .option("engine.WarnInterpreterOnly", false.toString)
        .build()
  }

  final class JSContextImpl(ctx: Context) extends JSContext {
    override def fromMap(m: Map[String, JSValue]): JSValue =
      Value.asValue(ProxyObject.fromMap(m.asJava))

    override def fromSeq(s: Seq[JSValue]): JSValue =
      Value.asValue(ProxyArray.fromList(s.asJava))

    override def fromString(s: String): JSValue =
      Value.asValue(s)

    override def fromBoolean(b: Boolean): JSValue =
      Value.asValue(b)

    override def fromDouble(d: Double): JSValue =
      Value.asValue(d)

    override def fromBigInt(i: BigInt): JSValue =
      ctx.eval("js", "BigInt").execute(i.bigInteger.toString)

    override def fromRegExp(r: JSRegExp): JSValue =
      ctx.eval("js", "RegExp").execute(r.source, r.flags)

    override def fromNull: JSValue =
      Value.asValue(null)



    private def getBigInt(value: Value): Option[BigInt] =
      val result = ctx.eval("js", "x => (typeof(x) == 'bigint' ? x.toString() : null)").execute(value)
      if result.isNull then
        None
      else
        Some(BigInt(result.asString))
    end getBigInt

    private def isRegExp(value: Value): Boolean =
      ctx.eval("js", "x => x instanceof RegExp").execute(value).asBoolean()

    override def decode(value: JSValue): DecodedJSObject | DecodedJSArray | String | Boolean | Double | BigInt | JSRegExp =
      if value.isString then
        value.asString()
      else if value.isNumber then
        getBigInt(value).getOrElse(value.asDouble())
      else if value.isBoolean then
        value.asBoolean()
      else if value.isNull then
        null
      else if isRegExp(value) then
        JSRegExp(value.getMember("source").asString(), value.getMember("flags").asString())
      else if value.hasArrayElements then
        DecodedJSArray(
          (0L until value.getArraySize)
            .map { i => value.getArrayElement(i) }
        )
      else if value.hasMembers then
        DecodedJSObject(
          value.getMemberKeys
            .asScala
            .iterator
            .map { key => key -> value.getMember(key) }
            .toMap
        )
      else
        throw new Exception("Unexpected object type")
      end if


    override protected def generateImpl(value: JSValue): IO[JSGenerateException, String] =
      ZIO.attempt {
        val source = Source
          .newBuilder("js", astringGenerateScript, "astring_generate.mjs")
          .mimeType("application/javascript+module")
          .build()

        val generate = ctx.eval(source)

        generate.execute(value).asString()
      }.refineOrDie {
        case NonFatal(ex) => JSGenerateException(ex)
      }

    override protected def parseImpl(fileName: String, text: String): IO[JSParseException, JSValue] =
      ZIO.attempt {
        val source = Source
          .newBuilder("js", acornParseScript, "acorn_parse.mjs")
          .mimeType("application/javascript+module")
          .build()

        val parse = ctx.eval(source)

        val options = ProxyObject.fromMap(Map(
          "ecmaVersion" -> 2021,
          "sourceType" -> "module",
          "locations" -> true,
          "sourceFile" -> fileName,
        ).asJava)

        parse.execute(text, options)
      }.refineOrDie {
        case NonFatal(ex) => JSParseException(ex)
      }



    private val astringGenerateScript =
      """
        |import {generate} from "astring.mjs";
        |generate;
        |""".stripMargin


    private val acornParseScript =
      """
        |import {parse} from "acorn.mjs"
        |parse;
        |""".stripMargin
  }
end JSValueUtil

export JSValueUtil.*


