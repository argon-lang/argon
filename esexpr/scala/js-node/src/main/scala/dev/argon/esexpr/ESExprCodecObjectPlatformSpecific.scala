package dev.argon.esexpr

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import dev.argon.util.{*, given}


trait ESExprCodecObjectPlatformSpecific {
  def getConstructedInfo(expr: ESExpr): Option[ESExprCodec.ConstructedInfo] =
    if js.typeOf(expr) == "object" &&
      expr != null &&
      expr.asInstanceOf[js.Dictionary[js.Any]].get("type").contains("constructor")
    then
      val ctor = expr.asInstanceOf[Constructed]
      Some(ESExprCodec.ConstructedInfo(ctor.constructor, ctor.kwargs.toMap, ctor.args.toSeq))
    else
      None
    end if

  def constructedInfoToExpr(info: ESExprCodec.ConstructedInfo): ESExpr =
    new Constructed {
      val `type`: "constructed" = "constructed"
      override val constructor: String = info.constructor
      override val kwargs: js.Map[String, ESExpr] = info.kwargs.toJSMap
      override val args: js.Array[ESExpr] = info.args.toJSArray
    }

  given ESExprCodec[String] with
    override lazy val constructors: Set[String] = Set.empty

    override def encode(value: String): ESExpr =
      value

    override def decode(expr: ESExpr): Either[String, String] =
      expr match {
        case expr: String => Right(expr)
        case _ => Left("Expected a string")
      }
  end given

  given ESExprCodec[Boolean] with
    override lazy val constructors: Set[String] = Set.empty

    override def encode(value: Boolean): ESExpr =
      value

    override def decode(expr: ESExpr): Either[String, Boolean] =
      expr match {
        case expr: Boolean => Right(expr)
        case _ => Left("Expected a boolean")
      }
  end given
}
