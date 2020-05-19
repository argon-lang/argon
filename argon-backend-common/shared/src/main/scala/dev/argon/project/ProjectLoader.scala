package dev.argon.project

import java.io.IOException

import cats.implicits._
import shapeless.{Path => _, _}
import zio._
import zio.interop.catz.core._

trait ProjectLoader[-A, +B, +IOld, -I] {
  def loadProject[R, E](a: A)(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, B]
}

object ProjectLoader {

  def apply[A, B, IOld, I](implicit loader: ProjectLoader[A, B, IOld, I]): ProjectLoader[A, B, IOld, I] = loader

  object Implicits {

    implicit def identityLoader[A, IOld, I]: ProjectLoader[A, A, IOld, I] = new ProjectLoader[A, A, IOld, I] {
      override def loadProject[R, E](a: A)(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, A] =
        IO.succeed(a)
    }

    implicit def fileLoader[IOld, I]: ProjectLoader[SingleFile[IOld], SingleFile[I], IOld, I] = new ProjectLoader[SingleFile[IOld], SingleFile[I], IOld, I] {
      override def loadProject[R, E](a: SingleFile[IOld])(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, SingleFile[I]] =
        fileHandler.loadSingleFile(a.file).map(new SingleFile(_))
    }

    implicit def fileListLoader[IOld, I]: ProjectLoader[FileList[IOld], FileList[I], IOld, I] = new ProjectLoader[FileList[IOld], FileList[I], IOld, I] {
      override def loadProject[R, E](a: FileList[IOld])(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, FileList[I]] =
        ZIO.foreach(a.files)(fileHandler.loadSingleFile).map(new FileList(_))
    }

    implicit def fileGlobLoader[IOld, I]: ProjectLoader[FileGlob[IOld], FileGlob[I], IOld, I] = new ProjectLoader[FileGlob[IOld], FileGlob[I], IOld, I] {
      override def loadProject[R, E](a: FileGlob[IOld])(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, FileGlob[I]] =
        fileHandler.loadGlobList(a.files).map(new FileGlob(_))
    }

    implicit def optionLoader[A, B, IOld, I](implicit innerLoader: ProjectLoader[A, B, IOld, I]): ProjectLoader[Option[A], Option[B], IOld, I] = new ProjectLoader[Option[A], Option[B], IOld, I] {
      override def loadProject[R, E](a: Option[A])(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, Option[B]] =
        ZIO.foreach(a)(innerLoader.loadProject(_))
    }

    implicit def hconsLoader[AHead, ATail <: HList, BHead, BTail <: HList, IOld, I](implicit headLoader: ProjectLoader[AHead, BHead, IOld, I], tailLoader: ProjectLoader[ATail, BTail, IOld, I]): ProjectLoader[AHead :: ATail, BHead :: BTail, IOld, I] =
      new ProjectLoader[AHead :: ATail, BHead :: BTail, IOld, I] {
        override def loadProject[R, E](a: AHead :: ATail)(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, BHead :: BTail] =
          headLoader.loadProject(a.head).flatMap { bHead =>
            tailLoader.loadProject(a.tail).map { bTail =>
              bHead :: bTail
            }
          }
      }

    implicit def productGenericLoader[A[_] <: Product, T1, T2, L1 <: HList, L2 <: HList, IOld, I](implicit gen1: Generic.Aux[A[T1], L1], gen2: Generic.Aux[A[T2], L2], loader: ProjectLoader[L1, L2, IOld, I]): ProjectLoader[A[T1], A[T2], IOld, I] =
      new ProjectLoader[A[T1], A[T2], IOld, I] {
        override def loadProject[R, E](a: A[T1])(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, A[T2]] =
          loader.loadProject(gen1.to(a)).map(gen2.from)
      }

    implicit def mapLoader[K1, K2, V1, V2, IOld, I](implicit keyLoader: ProjectLoader[K1, K2, IOld, I], valueLoader: ProjectLoader[V1, V2, IOld, I]): ProjectLoader[Map[K1, V1], Map[K2, V2], IOld, I] =
      new ProjectLoader[Map[K1, V1], Map[K2, V2], IOld, I] {
        override def loadProject[R, E](a: Map[K1, V1])(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, Map[K2, V2]] =
          a
          .toVector
          .traverse { case (k, v) =>
            for {
              k2 <- keyLoader.loadProject(k)
              v2 <- valueLoader.loadProject(v)
            } yield k2 -> v2
          }
          .map(_.toMap)
      }

  }



}
