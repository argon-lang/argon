package dev.argon.esexpr

import scala.jdk.CollectionConverters.*

trait ESExprCodecObjectPlatformSpecific {
  def constructedInfoToExpr(info: ESExprCodec.ConstructedInfo): ESExpr =
    ESExpr.Constructed(info.constructor, info.kwargs.asJava, info.args.asJava)

  def getConstructedInfo(expr: ESExpr): Option[ESExprCodec.ConstructedInfo] =
    expr match {
      case expr: ESExpr.Constructed =>
        Some(ESExprCodec.ConstructedInfo(expr.constructor(), expr.kwargs().asScala.toMap, expr.arguments().asScala.toSeq))

      case _ => None
    }

  given ESExprCodec[String] with
    override lazy val constructors: Set[String] = Set.empty

    override def encode(value: String): ESExpr =
      ESExpr.Str(value)

    override def decode(expr: ESExpr): Either[String, String] =
      expr match {
        case expr: ESExpr.Str => Right(expr.s())
        case _ => Left("Expected a string")
      }
  end given

  given ESExprCodec[Boolean] with
    override lazy val constructors: Set[String] = Set.empty

    override def encode(value: Boolean): ESExpr =
      ESExpr.Bool(value)

    override def decode(expr: ESExpr): Either[String, Boolean] =
      expr match {
        case expr: ESExpr.Bool => Right(expr.b())
        case _ => Left("Expected a boolean")
      }
  end given


}
