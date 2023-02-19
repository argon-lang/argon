package dev.argon.plugins.tube

import dev.argon.compiler.{Context, HasContext}
import dev.argon.compiler.tube.{ArTubeC, TubeImporter}
import dev.argon.io.ZipFileResource
import dev.argon.options.OptionDecoder
import dev.argon.plugin.TubeLoader
import dev.argon.plugin.tube.{SerializedTube, TubeReaderFactory}
import zio.*

final class TubeZipTubeLoader[R, E >: TubeError, ContextOptions] extends TubeLoader[R, E, ContextOptions] {
  override type LibOptions = TubeLibOptions[R, E, ContextOptions]


  override def libOptionDecoder(using OptionDecoder[R, E, ContextOptions]): OptionDecoder[R, E, LibOptions] =
    summon[OptionDecoder[R, E, LibOptions]]

  override def load
  (ctx: Context { type Env = R; type Error = E; type Options = ContextOptions })
  (tubeImporter2: TubeImporter & HasContext[ctx.type])
  (libOptions: LibOptions)
  : ZIO[ctx.Env & Scope, ctx.Error, ArTubeC & HasContext[ctx.type]] =
    libOptions.tube.asZip
      .flatMap { zip =>
        new TubeReaderFactory {
          override val context: ctx.type = ctx
          override protected val serialized: SerializedTube[context.Env, context.Error] = ZipFileTube(zip)
          override protected val tubeImporter: TubeImporter & HasContext[context.type] = tubeImporter2
        }.create
      }
      .flatMap { _.asTube }

}
