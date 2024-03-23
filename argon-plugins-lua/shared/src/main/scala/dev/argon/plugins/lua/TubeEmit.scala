package dev.argon.plugins.lua

import dev.argon.plugin.vm.*
import dev.argon.compiler.ModulePath as CModulePath
import zio.*
import cats.data.NonEmptySeq

trait TubeEmit extends TubeEmitBase {
  import context.Comp

  def emitTube: Comp[AST.Chunk] =
    for
      metadata <- currentTube.metadata()
      modules <- ZIO.foreach(metadata.modules) { modulePath =>
        for
          module <- currentTube.getModule(modulePath)
          moduleExpr = AST.MemberAccessIndex(
            AST.NameExp("modules"),
            AST.StringLiteral(CModulePath(modulePath.path).encode)
          )
          modStats <- emitModule(moduleExpr, module)
        yield AST.Assignment(Seq(moduleExpr),  Seq(toArrayExp(Seq()))) +: modStats
      }

    yield AST.Chunk(AST.Block(
      Seq(
        AST.LocalDeclaration(
          NonEmptySeq.of(AST.VariableBinding("ArgonRuntime", AST.Attrib.Empty)),
          Seq(AST.SimpleFunctionCall(AST.NameExp("require"), Seq(AST.StringLiteral("ArgonRuntime"))))
        ),
        AST.LocalDeclaration(NonEmptySeq.of(AST.VariableBinding("modules", AST.Attrib.Empty)), Seq(AST.TableConstructor(Seq.empty)))
      ) ++
        modules.flatten ++
        Seq(
          AST.Return(Seq(AST.NameExp("modules")))
        )
    ))

  private def emitModule(moduleExpr: AST.PrefixExp, module: VmModule): Comp[Seq[AST.Stat]] =
    new ModuleEmit {
      override val context: TubeEmit.this.context.type = TubeEmit.this.context
      override val currentTube: VmTube[Env, Error, Externs] = TubeEmit.this.currentTube
      override val currentModule: VmModule = module
    }.emitModule(moduleExpr)

}


