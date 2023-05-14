package dev.argon.plugin.tube

import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.*
import zio.*

object TubeSerializer {
  def ofImplementation
  (context: Context)
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  : ZIO[context.Env, context.Error, SerializedTubePlus[context.Env, context.Error, context.ExternMethodImplementation, context.ExternFunctionImplementation, context.ExternClassConstructorImplementation]] =
    TubeWriterImplementation(context)(tube).flatMap(SerializedTubeImpl(_))

  def ofInterface(context: Context)(tube: ArTubeC & HasContext[context.type]): ZIO[context.Env, context.Error, SerializedTube[context.Env, context.Error]] =
    tube.withHasImplementation(
      whenImplementation = TubeWriterFactoryInterfaceOfImplementation(context),
      whenInterface = TubeWriterFactoryInterfaceOnly(context),
    ).flatMap(SerializedTubeImpl(_))
}
