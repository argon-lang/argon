package dev.argon.plugins.tube

import dev.argon.compiler.{Context, HasContext}
import dev.argon.compiler.tube.{ArTubeC, TubeImporter}
import dev.argon.io.ZipFileResource
import dev.argon.options.OptionDecoder
import dev.argon.plugin.TubeLoader
import dev.argon.plugins.tube.reader.{TubeReaderBase, TubeReaderFactory}
import zio.*

object TubeZipTubeLoader extends TubeLoader[TubeError] {
  override type LibOptions[-R, +E, ContextOptions] = TubeLibOptions[R, E, ContextOptions]


  override def libOptionDecoder[R, E >: TubeError, ContextOptions](using OptionDecoder[R, E, ContextOptions]): OptionDecoder[R, E, LibOptions[R, E, ContextOptions]] =
    summon[OptionDecoder[R, E, LibOptions[R, E, ContextOptions]]]

  override def load
  (ctx: Context { type Error >: TubeError })
  (tubeImporter2: TubeImporter & HasContext[ctx.type])
  (libOptions: LibOptions[ctx.Env, ctx.Error, ctx.Options])
  : ZIO[ctx.Env & Scope, ctx.Error, ArTubeC & HasContext[ctx.type]] =
    for
      zipFile <- libOptions.tube.asZip
      reader <- (new TubeReaderFactory {
        override val context: ctx.type = ctx
        override protected val tubeImporter: TubeImporter & HasContext[ctx.type] = tubeImporter2
        override protected val zip: ZipFileResource.Zip[context.Env, context.Error] = zipFile
      }).create : ctx.Comp[TubeReaderBase & HasContext[ctx.type]]
      tube <- reader.asTube : ctx.Comp[ArTubeC & HasContext[ctx.type]]
    yield tube
}
