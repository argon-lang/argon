package com.mi3software.argon.compiler

sealed trait MetaClass[TContext <: Context, +TClass <: ArClass[TContext]]
final case class MetaClassDirect[TContext <: Context, +TClass <: ArClass[TContext]](metaClass: TClass) extends MetaClass[TContext, TClass]
final case class MetaClassMetaClass[TContext <: Context, +TClass <: ArClass[TContext]]() extends MetaClass[TContext, TClass]
