package dev.argon.plugins.lua

import dev.argon.util.{*, given}
import dev.argon.compiler.*
import dev.argon.ast.IdentifierExpr
import dev.argon.plugins.lua.ExprEmit.OutputMode
import zio.*
import zio.stm.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given

import scala.reflect.TypeTest

trait ModuleEmit extends ModuleEmitBase {
  def emitModule(moduleExpr: AST.PrefixExp): Comp[Seq[AST.Stat]] =
    for
      exports <- currentModule.exports

      nameTableStats =
        exports
          .iterator
          .map { (id, _, _) => id }
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

      elementStats <- ZIO.foreach(exports)(emitElement(moduleExpr))
    yield nameTableStats ++ elementStats


  private def emitElement(moduleExpr: AST.PrefixExp)(name: Option[IdentifierExpr], sig: ErasedSignature, exp: ModuleExport): Comp[AST.Stat] =
    for
      elementExp <- exp match {
        case f: VMFunction => emitFunction(f)
      }
    yield AST.Assignment(
      Seq(AST.MemberAccessIndex(
        AST.MemberAccessIndex(
          moduleExpr,
          getIdentifierKeyExprMemo(name),
        ),
        getErasedSigKeyExprMemo(sig),
      )),
      Seq(elementExp)
    )

  private trait ExprEmitCommon extends ExprEmit {
    override val context: ModuleEmit.this.context.type = ModuleEmit.this.context
    override val plugin: ModuleEmit.this.plugin.type = ModuleEmit.this.plugin
    override val currentTube: VMTube = ModuleEmit.this.currentTube
    override val currentModule: VMModule = ModuleEmit.this.currentModule
  }

  private def emitFunction(f: VMFunction): Comp[AST.Exp] =
    f.implementation match {
      case Some(impl) =>
        impl.flatMap {
          case FunctionImplementation.Instructions(body) =>
            for
              rm <- TMap.empty[Register, String].commit
              bm <- TMap.empty[UniqueIdentifier, String].commit


              func <- (new ExprEmitCommon {
                override val regMapping: TMap[Register, String] = rm
                override val blockMapping: TMap[UniqueIdentifier, String] = bm
              }).emit(body)
            yield func

          case FunctionImplementation.Extern(impl) =>
            val extern = (impl : ZEnvironment[LuaExternImplementation]).get[LuaExternImplementation]
            ZIO.succeed(
              AST.MemberAccessIndex(
                AST.SimpleFunctionCall(
                  AST.NameExp("require"),
                  Seq(AST.StringLiteral(extern.importPath))
                ),
                AST.StringLiteral(extern.memberName)
              )
            )
        }

      case None => ???
    }

}


