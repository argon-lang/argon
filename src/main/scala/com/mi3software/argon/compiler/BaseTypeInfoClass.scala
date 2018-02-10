package com.mi3software.argon.compiler

final case class BaseTypeInfoClass[TClass, TTrait](baseClass: Option[TClass], baseTraits: Vector[TTrait])
