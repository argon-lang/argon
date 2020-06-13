package dev.argon.compiler.lookup

import dev.argon.compiler.core.MemberName

sealed trait LookupDescription

object LookupDescription {

  final case class Identifier(name: String) extends LookupDescription
  final case class Operator(op: String) extends LookupDescription
  final case class Call(methodDesc: LookupDescription) extends LookupDescription
  case object Other extends LookupDescription
  final case class Member(objectDesc: LookupDescription, memberName: MemberName) extends LookupDescription

}
