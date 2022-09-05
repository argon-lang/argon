package dev.argon.compiler.expr

import dev.argon.compiler.{Context, HasContext}
import dev.argon.parser.IdentifierExpr
import dev.argon.compiler.module.*

type Imports[TContext <: Context] = Map[Option[IdentifierExpr], Seq[ModuleElementC[TContext, ?]]]
type ImportsWithModule[TContext <: Context] = Map[Option[IdentifierExpr], Seq[(ArModuleC & HasContext[TContext], ModuleElementC[TContext, ?])]]
