package dev.argon.backend.js

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler.loaders.ResourceIndicator
import zio._

private[js] trait JSEmitterBase {
  val context: JSContext

  import JSDSL._

  val currentModuleVarName = id"currentModule"

  trait VariableLoader {
    def loadVariable: JSExpression
    def storeVariable(value: JSExpression): Option[JSExpression]
    def initializeVariable(value: JSExpression): Option[JSStatement]
  }

  object VariableLoader {
    def fromExpr(expr: JSExpression): VariableLoader = new VariableLoader {
      override def loadVariable: JSExpression = expr
      override def storeVariable(value: JSExpression): Option[JSExpression] = None
      override def initializeVariable(value: JSExpression): Option[JSStatement] = None
    }
  }

  type VarMap = Map[VariableId, VariableLoader]

  final case class ArModuleElements
  (
    classes: Vector[JSExpression],
    traits: Vector[JSExpression],
    dataConstructors: Vector[JSExpression],
    functions: Vector[JSExpression],
  )

  final case class EmitEnv
  (
    module: ArModule[context.type, DeclarationPayloadSpecifier],
    modulePairs: Vector[(ArModule[context.type, ReferencePayloadSpecifier], JSIdentifier)],
    unnamedSymbols: Ref[Map[GlobalId, JSIdentifier]],
    nextSymbolId: Ref[Int],
    varMap: VarMap,
  )
  type Emit[+A] = RComp[EmitEnv, A]
  type UEmit[+A] = URIO[EmitEnv, A]

  def addToVarMap[E, A](mappings: (VariableId, VariableLoader)*)(action: ZIO[EmitEnv, E, A]): ZIO[EmitEnv, E, A] =
    action.provideSome[EmitEnv] { emitEnv =>
      emitEnv.copy(varMap = emitEnv.varMap ++ mappings)
    }

  def getVariableLoader(id: VariableId): Emit[VariableLoader] =
    ZIO.accessM[EmitEnv] { emitEnv =>
      requireSome(emitEnv.varMap.get(id))
    }

  def requireSome[A](opt: Option[A]): Comp[A] =
    IO.fromOption(opt)
      .mapError { _ => CompilationError.EmitError(CompilationMessageSource.EmitPhase()) }

}
