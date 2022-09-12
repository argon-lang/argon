package dev.argon.plugins.tube

import dev.argon.compiler.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.io.{Resource, ZipFileResource}
import dev.argon.options.OptionCodec
import zio.*
import zio.stream.ZStream

object TubeWriterResource {
  def apply[IsImpl <: Boolean]
  (ctx: Context { type Error >: TubeError })
  (options: IsImpl match {
    case true => ctx.Options
    case false => Unit
  })
  (createImpl: Queue[Exit[Option[ctx.Error], ZipFileResource.Entry[ctx.Env, ctx.Error]]] => UIO[TubeWriterBase { val context: ctx.type; type IsImplementation = IsImpl }])
  (using optionsCodec: OptionCodec[ctx.Env, ctx.Error, ctx.Options])
  : ZipFileResource[ctx.Env, ctx.Error] =
    new ZipFileResource[ctx.Env, ctx.Error] with ZipFileResource.Impl[ctx.Env, ctx.Error] with Resource.WithoutFileName:
      override def asZip: ZIO[ctx.Env & Scope, ctx.Error, ZipFileResource.Zip[ctx.Env, ctx.Error]] =
        ZIO.succeed(
          new ZipFileResource.Zip[ctx.Env, ctx.Error] {
            override def getEntry(path: String): ZIO[ctx.Env & Scope, ctx.Error, Option[ZipFileResource.Entry[ctx.Env, ctx.Error]]] =
              entries.find(_.path == path).runHead

            override def entries: ZStream[ctx.Env, ctx.Error, ZipFileResource.Entry[ctx.Env, ctx.Error]] =
              ZStream.unwrap(
                for
                  queue <- Queue.unbounded[Exit[Option[ctx.Error], ZipFileResource.Entry[ctx.Env, ctx.Error]]]
                  writer <- createImpl(queue)
                  _ <- writer.emitTube(options).asSomeError.foldCauseZIO[ctx.Env, ctx.Error, Any](
                    failure = cause => queue.offer(Exit.failCause(cause)),
                    success = _ => queue.offer(Exit.fail(None)),
                  ).fork
                yield ZStream.fromQueue(queue).flattenExitOption
              )
          }
        )
    end new
}
