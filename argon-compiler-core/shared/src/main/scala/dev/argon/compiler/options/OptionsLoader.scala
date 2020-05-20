package dev.argon.compiler.options

import cats.implicits._
import shapeless.{Path => _, _}
import zio._
import zio.interop.catz.core._

trait OptionsLoader[-A, +B, +IOld, -I] {
  def loadOptions[R, E](a: A)(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, B]
}

object OptionsLoader {

  def apply[A, B, IOld, I](implicit loader: OptionsLoader[A, B, IOld, I]): OptionsLoader[A, B, IOld, I] = loader

  object Implicits {

    implicit def identityLoader[A, IOld, I]: OptionsLoader[A, A, IOld, I] = new OptionsLoader[A, A, IOld, I] {
      override def loadOptions[R, E](a: A)(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, A] =
        IO.succeed(a)
    }

    implicit def optionLoader[A, B, IOld, I](implicit innerLoader: OptionsLoader[A, B, IOld, I]): OptionsLoader[Option[A], Option[B], IOld, I] =
      new OptionsLoader[Option[A], Option[B], IOld, I] {
        override def loadOptions[R, E](a: Option[A])(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, Option[B]] =
          ZIO.foreach(a)(innerLoader.loadOptions(_))
      }

    implicit def fileLoader[IOld, I]: OptionsLoader[SingleFile[IOld], SingleFile[I], IOld, I] = new OptionsLoader[SingleFile[IOld], SingleFile[I], IOld, I] {
      override def loadOptions[R, E](a: SingleFile[IOld])(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, SingleFile[I]] =
        fileHandler.loadSingleFile(a.file).map(new SingleFile(_))
    }

    implicit def fileListLoader[IOld, I]: OptionsLoader[FileList[IOld], FileList[I], IOld, I] = new OptionsLoader[FileList[IOld], FileList[I], IOld, I] {
      override def loadOptions[R, E](a: FileList[IOld])(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, FileList[I]] =
        ZIO.foreach(a.files)(fileHandler.loadSingleFile).map(new FileList(_))
    }

    implicit def fileGlobLoader[IOld, I]: OptionsLoader[FileGlob[IOld], FileGlob[I], IOld, I] = new OptionsLoader[FileGlob[IOld], FileGlob[I], IOld, I] {
      override def loadOptions[R, E](a: FileGlob[IOld])(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, FileGlob[I]] =
        fileHandler.loadGlobList(a.files).map(new FileGlob(_))
    }

    implicit def hconsLoader[AHead, ATail <: HList, BHead, BTail <: HList, IOld, I](implicit headLoader: OptionsLoader[AHead, BHead, IOld, I], tailLoader: OptionsLoader[ATail, BTail, IOld, I]): OptionsLoader[AHead :: ATail, BHead :: BTail, IOld, I] =
      new OptionsLoader[AHead :: ATail, BHead :: BTail, IOld, I] {
        override def loadOptions[R, E](a: AHead :: ATail)(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, BHead :: BTail] =
          headLoader.loadOptions(a.head).flatMap { bHead =>
            tailLoader.loadOptions(a.tail).map { bTail =>
              bHead :: bTail
            }
          }
      }

    implicit def productGenericLoader[A[_] <: Product, T1, T2, L1 <: HList, L2 <: HList, IOld, I](implicit gen1: Generic.Aux[A[T1], L1], gen2: Generic.Aux[A[T2], L2], loader: OptionsLoader[L1, L2, IOld, I]): OptionsLoader[A[T1], A[T2], IOld, I] =
      new OptionsLoader[A[T1], A[T2], IOld, I] {
        override def loadOptions[R, E](a: A[T1])(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, A[T2]] =
          loader.loadOptions(gen1.to(a)).map(gen2.from)
      }

    implicit def mapLoader[K1, K2, V1, V2, IOld, I](implicit keyLoader: OptionsLoader[K1, K2, IOld, I], valueLoader: OptionsLoader[V1, V2, IOld, I]): OptionsLoader[Map[K1, V1], Map[K2, V2], IOld, I] =
      new OptionsLoader[Map[K1, V1], Map[K2, V2], IOld, I] {
        override def loadOptions[R, E](a: Map[K1, V1])(implicit fileHandler: OptionsFileHandler[R, E, IOld, I]): ZIO[R, E, Map[K2, V2]] =
          a
          .toVector
          .traverse { case (k, v) =>
            for {
              k2 <- keyLoader.loadOptions(k)
              v2 <- valueLoader.loadOptions(v)
            } yield k2 -> v2
          }
          .map(_.toMap)
      }

  }



}
