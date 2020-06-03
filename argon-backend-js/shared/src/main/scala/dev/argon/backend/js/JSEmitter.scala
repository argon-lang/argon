package dev.argon.backend.js

import dev.argon.compiler._
import cats.{Id => _, _}
import cats.implicits._
import cats.data.NonEmptyList
import shapeless.Id
import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler.core._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.lookup.LookupNames
import dev.argon.compiler.types.TypeSystem
import dev.argon.compiler.vtable._
import dev.argon.util.NamespacePath
import zio._
import zio.interop.catz.core._
import zio.stream._

private[js] final class JSEmitter[TContext <: JSContext with Singleton, I <: ResourceIndicator: Tag]
(
  val context: TContext,
  val inject: JSInjectCode[Id, I]
) extends JSEmitterModule[I]
