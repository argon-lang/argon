package dev.argon.armodule.loader

import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}
import dev.argon.compiler.{core, _}
import dev.argon.compiler.core.{ErasedSignature, _}
import dev.argon.compiler.lookup._
import dev.argon.compiler.loaders.{ModuleLoader, UnlinkedModule}
import dev.argon.compiler.types._
import dev.argon.{module => ArgonModule}
import dev.argon.util._
import cats.{Id => _, _}
import cats.evidence.{===, Is}
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.implicits._
import dev.argon.armodule.ModulePaths
import dev.argon.module.{GlobalDeclarationElement, ModuleFormatVersion, ModuleReferencesList}
import dev.argon.compiler.expr._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.io.{FileNameUtil, ZipFileReader}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import shapeless.ops.nat.{LT, Pred, ToInt}
import shapeless.{Id, Nat, Sized, Succ, _0}
import shapeless.syntax.sized._
import zio.{IO, Managed, URIO, ZIO, ZManaged}
import zio.stream._
import zio.interop.catz.core._
import ModuleCreatorCommon._
import dev.argon.compiler.output.ArgonModuleSerialized
import dev.argon.io.fileio.ZipRead
import dev.argon.util.MaybeBlocking

import scala.collection.immutable.{Map, Vector}
import zio.NeedsEnv.needsEnv

object ArgonModuleLoader {

  def make: URIO[ZipRead with MaybeBlocking, ModuleLoader] =
    for {
      protoBufEnv <- ZIO.environment[MaybeBlocking]
      zipRead <- ZIO.access[ZipRead](_.get)
    } yield new ModuleLoader {
      override val supportedExtensions: Seq[String] = Seq("armodule")

      override def loadResource(fileName: String): CompManaged[Option[ArgonModuleSerialized]] =
        zipRead.openZipFile(fileName).catchAll(Compilation.unwrapThrowableManaged).mapM { zip =>
          ZipModuleSource.tryOpen(zip.catchAll(Compilation.unwrapThrowable)).provide(protoBufEnv)
        }

  }


}
