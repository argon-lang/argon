package dev.argon.build

import dev.argon.compiler.*
import esexpr.ESExprCodec
import zio.*
import zio.stm.*
import java.io.IOException

object TubeImporterImpl {
  def apply(ctx: CContext): UIO[TubeImporter & LoadTube & HasContext[ctx.type]] =
    for
      tubes <- TMap.empty[TubeName, ArTubeC & HasContext[ctx.type]].commit
    yield new TubeImporter with LoadTube {
      override val context: ctx.type = ctx
      import context.given

      override def getTube(tubeName: TubeName): Comp[ArTube] =
        tubes.get(tubeName).commit
          .some
          .orElseFail(UnknownTube(tubeName))
          
      override def loadTube(tube: ArTubeC & HasContext[context.type]): UIO[Unit] =
        tubes.put(tube.name, tube).commit
    }
}
