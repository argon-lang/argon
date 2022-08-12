package dev.argon.plugins.js

import scala.scalajs.js
import scala.scalajs.js.JSConverters.given
import zio.*

import java.util.Objects
import scala.util.control.NonFatal


object JSValueUtil:
  opaque type JSValue = js.Any | Null

  type JSRegExp = js.RegExp

  trait JSContextPlatformSpecific {
    def make: URIO[Scope, JSContext] =
      ZIO.succeed { JSContextImpl() }
  }

  object JSValue {
    def fromMap(m: Map[String, JSValue]): JSValue = m.toJSDictionary
    def fromSeq(s: Seq[JSValue]): JSValue = s.toJSArray

    def toMap(value: JSValue): Option[Map[String, JSValue]] =
      Some(value.asInstanceOf[js.Dictionary[JSValue]].toMap)

    def toSeq(value: JSValue): Option[Seq[JSValue]] =
      value match {
        case value: js.Array[?] =>
          Some(value.asInstanceOf[js.Array[JSValue]].toSeq)

        case _ => None
      }
  }



  final class JSContextImpl() extends JSContext {
    override def fromMap(m: Map[String, JSValue]): JSValue =
      m.toJSDictionary

    override def fromSeq(s: Seq[JSValue]): JSValue =
      s.toJSArray : js.Any

    override def fromString(s: String): JSValue =
      s

    override def fromBoolean(b: Boolean): JSValue =
      b

    override def fromDouble(d: Double): JSValue =
      d

    override def fromBigInt(i: BigInt): JSValue =
      js.BigInt(i.toString)

    override def fromRegExp(r: JSRegExp): JSValue =
      r

    override def fromNull: JSValue =
      null

    override def decode(value: JSValue): DecodedJSObject | DecodedJSArray | String | Boolean | Double | BigInt | JSRegExp | Null =
      (value : Matchable | Null) match {
        case value: String => value
        case value: Boolean => value
        case value: Double => value
        case _ if js.typeOf(value) == "bigint" =>
          BigInt(value.toString())
        case value: js.RegExp => value
        case value: js.Array[?] => DecodedJSArray(value.asInstanceOf[js.Array[JSValue]].toSeq)
        case value if js.isUndefined(value) || Objects.isNull(value) => null
        case _ => DecodedJSObject(value.asInstanceOf[js.Dictionary[JSValue]].toMap)
      }


    override protected def generateImpl(value: JSValue): IO[JSGenerateError, String] =
      ZIO.attempt {
        Astring.generate(value.nn)
      }.refineOrDie {
        case NonFatal(ex) => JSGenerateError(ex)
      }

    override protected def parseImpl(fileName: String, text: String): IO[JSParseError, JSValue] =
      ZIO.attempt {
        val options = new Acorn.Options {
          override val ecmaVersion = 2022
          override val sourceType = "module"
          override val locations = true
          override val sourceFile = fileName
        }

        Acorn.parse(text, options)
      }.refineOrDie {
        case NonFatal(ex) => JSParseError(ex)
      }

  }

end JSValueUtil

export JSValueUtil.*


