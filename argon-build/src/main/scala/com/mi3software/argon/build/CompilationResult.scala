package com.mi3software.argon.build

import com.mi3software.argon.compiler._
import scalaz._

final case class CompilationResult[TOutput](messages: Set[CompilationMessageNonFatal], result: NonEmptyList[CompilationError] \/ TOutput)
