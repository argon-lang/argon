package dev.argon.plugins.scheme

import dev.argon.plugin.scalaApi.*
import esexpr.ESExprCodec

private[scheme] sealed abstract class SchemeExterns
private[scheme] object SchemeExterns {
  final case class ExternFunction(library: Seq[String], id: String) extends SchemeExterns with ExternKindFunction derives ESExprCodec
  final case class ExternRefFunction(library: Seq[String], id: String) extends SchemeExterns with ExternRefKindFunction derives ESExprCodec
  final case class ExternRefRecord(library: Seq[String], id: String) extends SchemeExterns with ExternRefKindRecord derives ESExprCodec
}

