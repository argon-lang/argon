package com.mi3software.argon.compiler

sealed trait LookupDescription

object LookupDescription {

  final case class Identifier(name: String) extends LookupDescription
  final case class Call(methodDesc: LookupDescription) extends LookupDescription
  final case class Member(objectDesc: LookupDescription, memberName: MemberName) extends LookupDescription

}
