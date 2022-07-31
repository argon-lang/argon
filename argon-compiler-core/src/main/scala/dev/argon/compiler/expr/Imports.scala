package dev.argon.compiler.expr

import dev.argon.compiler.Context
import dev.argon.parser.IdentifierExpr
import dev.argon.compiler.module.{ModuleName, ModuleElementC}

type Imports[TContext <: Context] = Map[IdentifierExpr, Seq[ModuleElementC[TContext, ?]]]