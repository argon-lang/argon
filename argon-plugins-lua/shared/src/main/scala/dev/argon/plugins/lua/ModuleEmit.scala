package dev.argon.plugins.lua

import dev.argon.util.{*, given}
import dev.argon.ast.IdentifierExpr
import dev.argon.plugin.vm.*
import zio.*
import zio.stm.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given

import scala.reflect.TypeTest

trait ModuleEmit extends ModuleEmitBase {
  def emitModule(moduleExpr: AST.PrefixExp): Comp[Seq[AST.Stat]] =
    val exports = currentModule.exports

    val nameTableStats =
      exports
        .iterator
        .map { _.name }
        .distinct
        .map { id =>
          AST.Assignment(
            Seq(AST.MemberAccessIndex(
              moduleExpr,
              getIdentifierKeyExprMemo(id)
            )),
            Seq(toArrayExp(Seq()))
          )
        }
        .toSeq

    for
      elementStats <- ZIO.foreach(exports)(emitElement(moduleExpr))
    yield nameTableStats ++ elementStats
  end emitModule



  private def emitElement(moduleExpr: AST.PrefixExp)(entry: VmModuleExportEntry): Comp[AST.Stat] =
    for
      elementExp <- entry.`export` match {
        case VmModuleExport.Function(funcId) =>
          currentTube.getFunctionDefinition(funcId)
            .flatMap(emitFunction(funcId))
      }
    yield AST.Assignment(
      Seq(AST.MemberAccessIndex(
        AST.MemberAccessIndex(
          moduleExpr,
          getIdentifierKeyExprMemo(entry.name),
        ),
        getErasedSigKeyExprMemo(entry.signature),
      )),
      Seq(elementExp)
    )

  private trait ExprEmitCommon extends ExprEmit {
    override val context: ModuleEmit.this.context.type = ModuleEmit.this.context
    override val currentTube: VmTube[Env, Error, Externs] = ModuleEmit.this.currentTube
    override val currentModule: VmModule = ModuleEmit.this.currentModule
  }

  private def emitFunction(funcId: BigInt)(f: VmFunction): Comp[AST.Exp] =
    f.implementation match {
      case Some(VmFunctionImplementation.Instructions(body)) =>
        for
          rn <- TSet.empty[String].commit

          func <- (new ExprEmitCommon {
            override val regNames: TSet[String] = rn
          }).emit(body)
        yield func

      case Some(VmFunctionImplementation.Extern) =>
        for
          impl <- currentTube.getFunctionExternImplementation(funcId)
          extern = (impl : ZEnvironment[LuaExternImplementation]).get[LuaExternImplementation]
        yield AST.MemberAccessIndex(
          AST.SimpleFunctionCall(
            AST.NameExp("require"),
            Seq(AST.StringLiteral(extern.importPath))
          ),
          AST.StringLiteral(extern.memberName)
        )

      case None => ???
    }

}


