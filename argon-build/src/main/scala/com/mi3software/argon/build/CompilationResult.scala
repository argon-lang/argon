package com.mi3software.argon.build

import com.mi3software.argon.compiler._
import scalaz._

final case class CompilationResult(messages: Set[CompilationMessageNonFatal], result: NonEmptyList[CompilationError] \/ CompilationOutput)
