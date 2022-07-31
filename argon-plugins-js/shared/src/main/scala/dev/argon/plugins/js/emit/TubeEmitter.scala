package dev.argon.plugins.js.emit

import dev.argon.compiler.*
import dev.argon.compiler.module.{ModuleName, ModulePath}
import dev.argon.compiler.definitions.HasDeclaration
import dev.argon.compiler.signature.ImportSpecifier
import dev.argon.compiler.tube.ArTubeC
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.io.*
import dev.argon.plugin.*
import dev.argon.plugins.js.*
import dev.argon.plugins.js.{JSOutput, JSProgramResource, estree}
import dev.argon.util.*
import zio.*
import zio.stm.*
import zio.stream.*

private[js] trait TubeEmitter extends EmitTubeCommon {
  val tube: ArTube with HasDeclaration[true]

  def emitTube: Comp[JSOutput[context.Env, context.Error]] =
    ZIO.succeed(JSOutput(
      `package` = toDirectoryStructure(emitModule),
    ))

  private def emitModule(path: ModulePath): JSProgramResource[context.Env, context.Error] =
    new JSProgramResource.Impl[context.Env, context.Error] with Resource.WithoutFileName:
      override def asModule: Comp[estree.Program] =
        for
          importMap <- TMap.empty[ImportSpecifier, String].commit
          additionalImportMap <- TMap.empty[ModuleName, TSet[String]].commit
          arModule <- tube.module(path)
          program <-
            new ModuleEmitter {
              override val context: TubeEmitter.this.context.type = TubeEmitter.this.context
              override val options: JSOptions[context.Env, context.Error] = TubeEmitter.this.options
              override val adapter: PluginContextAdapter.Aux[context.type, JSPlugin.type] = TubeEmitter.this.adapter
              override val tube: ArTube = TubeEmitter.this.tube
              override val imports: TMap[ImportSpecifier, String] = importMap
              override val additionalImports: TMap[ModuleName, TSet[String]] = additionalImportMap
              override val module: ArModule with HasDeclaration[true] = arModule
              override protected val vtableBuilder: VTableBuilder[context.type] = TubeEmitter.this.vtableBuilder
            }.program
        yield program
    end new


  private def toDirectoryStructure[R, E](f: ModulePath => JSProgramResource[R, E]): DirectoryResource[R, E, JSProgramResource] =
    def directory(files: UStream[ModuleFile]): DirectoryResource[R, E, JSProgramResource] =
      new DirectoryResource[R, E, JSProgramResource] with Resource.WithoutFileName {
        override def contents: ZStream[R, E, DirectoryEntry[R, E, JSProgramResource]] =
          val tmp: ZStream[R, E, DirectoryEntry[R, E, JSProgramResource]] =
            files.groupBy {
              case ModuleFile(head +: tail, file, path) => ZIO.succeed((Some(head), ModuleFile(tail, file, path)))
              case file => ZIO.succeed((None, file))
            } {
              case (Some(name), files) =>
                ZStream(DirectoryEntry.Subdirectory(name, directory(files)))

              case (None, files) =>
                files.map { case ModuleFile(_, file, path) => DirectoryEntry.File(file + ".js", f(path)) }
            }

          tmp
      }

    directory(ZStream.fromIterable(tube.modulePaths.iterator.map(getModuleFileName(tube)).toSeq))
  end toDirectoryStructure

}
