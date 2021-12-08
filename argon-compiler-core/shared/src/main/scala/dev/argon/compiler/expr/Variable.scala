package dev.argon.compiler.expr

import dev.argon.compiler._

sealed trait Variable[TContext <: Context] {
  
}

object Variable {
  given canEqualInstance[TContext <: Context]: CanEqual[Variable[TContext], Variable[TContext]] = CanEqual.derived
}

final case class LocalVariable[TContext <: Context](

) extends Variable[TContext]

