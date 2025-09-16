package dev.argon.backend

import dev.argon.compiler.Context
import dev.argon.compiler.Context.{Env0, Error0}
import dev.argon.tube.{ExternMap, SupportedPlatform}
import esexpr.ESExpr
import zio.EnvironmentTag

import java.io.IOException
import scala.reflect.TypeTest

class BackendContext[R <: Env0: EnvironmentTag, E >: Error0 | BackendException | IOException <: Matchable](using TypeTest[Any, E]) extends Context.Of[R, E] {

  class BackendContextImpls extends Implementations {
    override type TubeMetadata = (Seq[SupportedPlatform], Map[String, ESExpr])
    override type ExternFunction = ExternMap
    override type ExternMethod = ExternMap
  }

  override val implementations: BackendContextImpls = BackendContextImpls()
}
