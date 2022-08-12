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
      Context.newBuilder("js").nn
        .allowIO(true).nn
        .fileSystem(new ResourceFileSystem).nn
        .option("engine.WarnInterpreterOnly", false.toString).nn
        .build().nn
  }

  final class JSContextImpl(ctx: Context) extends JSContext {
    override def fromMap(m: Map[String, JSValue]): JSValue =
      Value.asValue(ProxyObject.fromMap(m.asJava)).nn

    override def fromSeq(s: Seq[JSValue]): JSValue =
      Value.asValue(ProxyArray.fromList(s.asJava)).nn

    override def fromString(s: String): JSValue =
      Value.asValue(s).nn

    override def fromBoolean(b: Boolean): JSValue =
      Value.asValue(b).nn

    override def fromDouble(d: Double): JSValue =
      Value.asValue(d).nn

    override def fromBigInt(i: BigInt): JSValue =
      ctx.eval("js", "BigInt").nn.execute(i.bigInteger.toString).nn

    override def fromRegExp(r: JSRegExp): JSValue =
      ctx.eval("js", "RegExp").nn.execute(r.source, r.flags).nn

    override def fromNull: JSValue =
      Value.asValue(null).nn



    private def getBigInt(value: Value): Option[BigInt] =
      val result = ctx.eval("js", "x => (typeof(x) == 'bigint' ? x.toString() : null)").nn.execute(value).nn
      if result.isNull then
        None
      else
        Some(BigInt(result.asString.nn))
    end getBigInt

    private def isRegExp(value: Value): Boolean =
      ctx.eval("js", "x => x instanceof RegExp").nn.execute(value).nn.asBoolean()

    override def decode(value: JSValue): DecodedJSObject | DecodedJSArray | String | Boolean | Double | BigInt | JSRegExp | Null =
      if value.isString then
        value.asString().nn
      else if value.isNumber then
        getBigInt(value).getOrElse(value.asDouble())
      else if value.isBoolean then
        value.asBoolean()
      else if value.isNull then
        null
      else if isRegExp(value) then
        JSRegExp(value.getMember("source").nn.asString().nn, value.getMember("flags").nn.asString().nn)
      else if value.hasArrayElements then
        DecodedJSArray(
          (0L until value.getArraySize)
            .map { i => value.getArrayElement(i).nn }
        )
      else if value.hasMembers then
        DecodedJSObject(
          value.getMemberKeys.nn
            .asScala
            .iterator
            .map { key => key -> value.getMember(key).nn }
            .toMap
        )
      else
        throw new Exception("Unexpected object type")
      end if


    override protected def generateImpl(value: JSValue): IO[JSGenerateError, String] =
      ZIO.attempt {
        val source = Source
          .newBuilder("js", astringGenerateScript, "astring_generate.mjs").nn
          .mimeType("application/javascript+module").nn
          .build().nn

        val generate = ctx.eval(source).nn

        generate.execute(value).nn.asString().nn
      }.refineOrDie {
        case NonFatal(ex) => JSGenerateError(ex)
      }

    override protected def parseImpl(fileName: String, text: String): IO[JSParseError, JSValue] =
      ZIO.attempt {
        val source = Source
          .newBuilder("js", acornParseScript, "acorn_parse.mjs").nn
          .mimeType("application/javascript+module").nn
          .build().nn

        val parse = ctx.eval(source).nn

        val options = ProxyObject.fromMap(Map(
          "ecmaVersion" -> 2022,
          "sourceType" -> "module",
          "locations" -> true,
          "sourceFile" -> fileName,
        ).asJava)

        parse.execute(text, options).nn
      }.refineOrDie {
        case NonFatal(ex) => JSParseError(ex)
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


