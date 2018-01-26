package com.mi3software.argon.util

final case class WithSource[+T](value: T, location: SourceLocation)
