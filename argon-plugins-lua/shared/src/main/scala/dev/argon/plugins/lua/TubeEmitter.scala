package dev.argon.plugins.lua

import dev.argon.argonvm.VM
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.module.ModulePath
import dev.argon.compiler.{Context, UsingContext}
import dev.argon.plugin.{PluginAdapter, PluginContext}
import zio.*

trait TubeEmitter[R <: LuaEnv, E >: LuaError] extends TubeEmitterBase[R, E] {
  import context.Comp

  def emitTube: Comp[AST.Chunk] =
    for
      modules <- ZIO.foreach(currentTube.modulePaths.toSeq) { modulePath =>
        for
          moduleExp <- emitModule(modulePath)
        yield AST.Field.NamedWithExp(
          getModulePathExpr(modulePath),
          moduleExp,
        )
      }
      
    yield AST.Chunk(AST.Block(Seq(
      AST.LocalDeclaration(
        Seq(("ArgonRuntime", AST.Attrib.Empty)),
        Seq(AST.SimpleFunctionCall(AST.NameExp("require"), Seq(AST.StringLiteral("Argon.Runtime"))))
      ),
      AST.Return(Seq(
        
        AST.FunctionDefinitionExp(
          Seq("rt"),
          hasRest = false,
          AST.Block(Seq(
            AST.Return(Seq(
              AST.MethodCall(
                AST.NameExp("rt"),
                "create_tube",
                Seq(
                  AST.TableConstructor(modules),
                ),
              ),
            )),
          )),
        ),
      )),
    )))

  private def emitModule(modulePath: ModulePath): Comp[AST.Exp] =
    currentTube.module(modulePath).flatMap { m =>
      new ModuleEmitter[R, E] {
        override val context: TubeEmitter.this.context.type = TubeEmitter.this.context
        override val plugin: TubeEmitter.this.plugin.type = TubeEmitter.this.plugin
        override val pluginAdapter: PluginAdapter[R, E, TubeEmitter.this.context.plugin.type, TubeEmitter.this.plugin.type] = TubeEmitter.this.pluginAdapter
        override val currentTube: ArTube & HasImplementation[true] = TubeEmitter.this.currentTube
        override val currentModule: ArModule & HasImplementation[true] = m
      }.emitModule
    }

}


