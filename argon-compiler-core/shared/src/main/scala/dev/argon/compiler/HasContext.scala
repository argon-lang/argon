package dev.argon.compiler

type HasContext[TContext <: Context] = UsingContext { val context: TContext }
