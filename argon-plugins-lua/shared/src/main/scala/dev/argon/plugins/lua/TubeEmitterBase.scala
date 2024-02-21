package dev.argon.plugins.lua

import dev.argon.compiler.UsingContext
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.module.ModulePath
import dev.argon.compiler.tube.TubeName
import dev.argon.plugin.{PluginAdapter, PluginContext}
import dev.argon.util.{*, given}

trait TubeEmitterBase[R <: LuaEnv, E >: LuaError] extends UsingContext {
  override val context: PluginContext[R, E, ?]
  val plugin: LuaPlugin[R, E]
  val pluginAdapter: PluginAdapter[R, E, context.plugin.type, plugin.type]
  val currentTube: ArTube & HasImplementation[true]

  protected def toArrayExp(values: Seq[AST.Exp]): AST.Exp =
    AST.TableConstructor(values.map(AST.Field.Positional.apply))

  protected def getTubeNameExpr(name: TubeName): AST.Exp =
    toArrayExp(name.name.toList.map(AST.StringLiteral.apply))

  protected def getTubeNameExprMemo(name: TubeName): AST.Exp =
    AST.MethodCall(
      AST.NameExp("rt"),
      "intern_tube_name",
      Seq(getTubeNameExpr(name)),
    )

  protected def getModulePathExpr(path: ModulePath): AST.Exp =
    toArrayExp(path.ids.toList.map(AST.StringLiteral.apply))

  protected def getModulePathExprMemo(path: ModulePath): AST.Exp =
    AST.MethodCall(
      AST.NameExp("rt"),
      "intern_module_path",
      Seq(getModulePathExpr(path)),
    )
}


