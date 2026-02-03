package dev.argon.build

import dev.argon.compiler.*
import java.io.IOException


type CompileStepEnv = Any
type CompileStepError = BuildError | IOException
type CContext = Context {
  type Env <: CompileStepEnv
  type Error >: CompileStepError
}
