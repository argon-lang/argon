package com.mi3software.argon.compiler

sealed trait MetaClass[+TClass]
final case class MetaClassDirect[+TClass](metaClass: TClass) extends MetaClass[TClass]
final case class MetaClassMetaClass() extends MetaClass[Nothing]
