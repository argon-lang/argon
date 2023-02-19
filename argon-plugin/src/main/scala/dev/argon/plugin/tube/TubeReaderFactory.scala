package dev.argon.plugin.tube

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.TubeImporter
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.io.ZipFileResource
import dev.argon.tube as t
import dev.argon.util.protobuf.given
import dev.argon.util.{*, given}
import zio.*

abstract class TubeReaderFactory extends UsingContext {
  override val context: Context { type Error >: InvalidTube }

  protected val serialized: SerializedTube[context.Env, context.Error]

  protected val tubeImporter: TubeImporter & HasContext[context.type]

  def create: Comp[TubeReaderBase & HasContext[context.type]] =
    for
      _ <- checkVersion
      metadata <- serialized.metadata
      vtBuilder <- VTableBuilder(TubeReaderFactory.this.context, tubeImporter)
      reader <- metadata.`type` match {
        case t.TubeType.Interface =>
          ZIO.succeed(new TubeReaderCommon(vtBuilder) {
            override type IsImplementation = true

            override protected def withHasImplementationImpl[A]
            (tube: ArTube & HasImplementation[true])
            (
              whenImplementation: tube.type & HasImplementation[true] => A,
              whenInterface: tube.type & HasImplementation[false] => A,
            ): A =
              whenImplementation(tube)

            override protected def ensureOptions(opt: Option[this.context.Options]): Comp[this.context.Options] =
              ZIO.fromEither(opt.toRight { InvalidTube("Missing options for implementation tube") })


            override protected def createImplementation[TImpl](currentTube: ArTube & HasImplementation[IsImplementation])(f: Option[this.context.Options] => Comp[Option[TImpl]]): Comp[TImpl] =
              f(Some(currentTube.options)).flatMap {
                case Some(impl) => ZIO.succeed(impl)
                case None => ZIO.fail(InvalidTube("Missing implementation in implementation tube"))
              }

            override protected def getMaybeImplementation[TImpl]: Comp[TImpl] => Comp[Option[TImpl]] =
              _.asSome
          })

        case t.TubeType.Implementation =>
          ZIO.succeed(new TubeReaderCommon(vtBuilder) {
            override type IsImplementation = false

            override protected def withHasImplementationImpl[A]
            (tube: ArTube & HasImplementation[false])
            (
              whenImplementation: tube.type & HasImplementation[true] => A,
              whenInterface: tube.type & HasImplementation[false] => A,
            ): A =
              whenInterface(tube)

            override protected def ensureOptions(opt: Option[this.context.Options]): Comp[Unit] =
              ZIO.unit

            override protected def createImplementation[TImpl](currentTube: ArTube & HasImplementation[IsImplementation])(f: Option[this.context.Options] => Comp[Option[TImpl]]): Comp[Option[TImpl]] =
              f(None)

            override protected def getMaybeImplementation[TImpl]: Comp[Option[TImpl]] => Comp[Option[TImpl]] =
              identity

          })

        case t.TubeType.Unrecognized(_) =>
          ZIO.fail(InvalidTube("Unknown tube type"))
      }
    yield reader


  private def checkVersion: Comp[Unit] =
    for
      version <- serialized.version
      defaultVersion = t.TubeProto.defaultTubeFormatVersion.get(t.TubeFormatVersion.scalaDescriptor.getOptions).get
      _ <- ZIO.fail(InvalidTube(s"Tube format ${version.major}.${version.minor} is newer than maximum supported version ${defaultVersion.major}.${defaultVersion.minor}"))
        .when(version.major > defaultVersion.major || (version.major == defaultVersion.major && version.minor > defaultVersion.minor))
    yield ()

  private abstract class TubeReaderCommon(override protected val vtableBuilder: VTableBuilder[context.type]) extends TubeReaderBase {
    override val context: TubeReaderFactory.this.context.type = TubeReaderFactory.this.context
    override protected val serialized: SerializedTube[context.Env, context.Error] = TubeReaderFactory.this.serialized
    override protected val tubeImporter: TubeImporter & HasContext[context.type] = TubeReaderFactory.this.tubeImporter
  }

}
