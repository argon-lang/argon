package com.mi3software.argon.parser


sealed trait VisibilityMode

object VisibilityMode {
  case object Unspecified extends VisibilityMode
  case object Private extends VisibilityMode
  case object Protected extends VisibilityMode
  case object Public extends VisibilityMode
  case object Internal extends VisibilityMode
  case object ProtectedInternal extends VisibilityMode
  case object PrivateInternal extends VisibilityMode
}
