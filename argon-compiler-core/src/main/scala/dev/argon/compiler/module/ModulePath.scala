package dev.argon.compiler.module

import dev.argon.parser.IdentifierExpr

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

final case class ModulePath(ids: Seq[String]) derives CanEqual {

  def urlEncode: String =
    ids.map { _.replace("%", "%25").nn.replace("/", "%2F") }.mkString("/")

  override def toString: String =
    ids.mkString("/")
}

object ModulePath {
  def urlDecode(pathStr: String): ModulePath =
    if pathStr.isEmpty then
      ModulePath(Seq.empty)
    else
      ModulePath(
        pathStr
          .split("/").nn
          .iterator
          .map { seg => URLDecoder.decode(seg, StandardCharsets.UTF_8).nn }
          .toSeq
      )
}
