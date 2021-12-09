package dev.argon.compiler.module

import dev.argon.parser.IdentifierExpr
import dev.argon.compiler.*

final case class ModuleEntryC[TContext <: Context](name: Option[IdentifierExpr], element: ModuleElementC[TContext])
