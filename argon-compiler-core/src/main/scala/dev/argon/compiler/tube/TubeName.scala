package dev.argon.compiler.tube

import dev.argon.util.*

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

final case class TubeName(name: NonEmptyList[String]) derives CanEqual {
  def urlEncode: String =
    name.toList.map { _.replace("%", "%25").nn.replace(".", "%2E") }.mkString(".")

  override def toString: String = name.toList.mkString(".")
}

object TubeName {
  def urlDecode(nameStr: String): Option[TubeName] =
    if nameStr == "" then
      None
    else
      NonEmptyList.fromList(
        nameStr
          .split("\\.").nn
          .iterator
          .map { seg => URLDecoder.decode(seg, "UTF-8").nn }
          .toList
      ).map(TubeName.apply)
}
