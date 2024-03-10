package dev.argon.plugins.lua

import dev.argon.compiler.*
import zio.*

trait TubeEmit extends TubeEmitBase {
  import context.Comp

  def emitTube: Comp[AST.Chunk] =
    for
      modules <- ZIO.foreach(currentTube.modules.toSeq) { (modulePath, module) =>
        val moduleExpr = AST.MemberAccessIndex(
          AST.NameExp("modules"),
          AST.StringLiteral(modulePath.encode)
        )

        for
          modStats <- emitModule(moduleExpr, module)
        yield AST.Assignment(Seq(moduleExpr),  Seq(toArrayExp(Seq()))) +: modStats
      }

    yield AST.Chunk(AST.Block(
      Seq(
        AST.LocalDeclaration(
          Seq(AST.VariableBinding("ArgonRuntime", AST.Attrib.Empty)),
          Seq(AST.SimpleFunctionCall(AST.NameExp("require"), Seq(AST.StringLiteral("ArgonRuntime"))))
        ),
        AST.LocalDeclaration(Seq(AST.VariableBinding("modules", AST.Attrib.Empty)), Seq(AST.TableConstructor(Seq.empty)))
      ) ++
        modules.flatten ++
        Seq(
          AST.Return(Seq(AST.NameExp("modules")))
        )
    ))

  private def emitModule(moduleExpr: AST.PrefixExp, module: VMModule): Comp[Seq[AST.Stat]] =
    new ModuleEmit {
      override val context: TubeEmit.this.context.type = TubeEmit.this.context
      override val plugin: TubeEmit.this.plugin.type = TubeEmit.this.plugin
      override val currentTube: VMTube = TubeEmit.this.currentTube
      override val currentModule: VMModule = module
    }.emitModule(moduleExpr)

}


