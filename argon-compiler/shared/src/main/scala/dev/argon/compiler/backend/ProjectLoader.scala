package dev.argon.compiler.backend

import java.io.{File, IOException}
import java.nio.file._

import cats._
import cats.implicits._
import dev.argon.io.{FileIO, FilenameManip}
import zio._
import shapeless.{Path => _, _}
import zio.stream.ZSink

trait ProjectLoader[A, B, I] {
  def loadProject[F[_]](a: A)(implicit monadInstance: Monad[F], fileHandler: ProjectFileHandler[F, I]): F[B]
}

object ProjectLoader {

  def apply[A, B, I](implicit loader: ProjectLoader[A, B, I]): ProjectLoader[A, B, I] = loader

  object Implicits {

    implicit def identityLoader[A, I]: ProjectLoader[A, A, I] = new ProjectLoader[A, A, I] {
      override def loadProject[F[_]](a: A)(implicit monadInstance: Monad[F], fileHandler: ProjectFileHandler[F, I]): F[A] =
        monadInstance.point(a)
    }

    implicit def fileLoader[I]: ProjectLoader[String, I, I] = new ProjectLoader[String, I, I] {
      override def loadProject[F[_]](a: String)(implicit monadInstance: Monad[F], fileHandler: ProjectFileHandler[F, I]): F[I] =
        fileHandler.loadSingleFile(a)
    }

    implicit def fileListLoader[I]: ProjectLoader[List[String], List[I], I] = new ProjectLoader[List[String], List[I], I] {
      override def loadProject[F[_]](a: List[String])(implicit monadInstance: Monad[F], fileHandler: ProjectFileHandler[F, I]): F[List[I]] =
        a.traverse(fileHandler.loadSingleFile)
    }

    implicit def hconsLoader[AHead, ATail <: HList, BHead, BTail <: HList, I](implicit headLoader: ProjectLoader[AHead, BHead, I], tailLoader: ProjectLoader[ATail, BTail, I]): ProjectLoader[AHead :: ATail, BHead :: BTail, I] =
      new ProjectLoader[AHead :: ATail, BHead :: BTail, I] {
        override def loadProject[F[_]](a: AHead :: ATail)(implicit monadInstance: Monad[F], fileHandler: ProjectFileHandler[F, I]): F[BHead :: BTail] =
          headLoader.loadProject(a.head)(monadInstance, fileHandler).flatMap { bHead =>
            tailLoader.loadProject(a.tail)(monadInstance, fileHandler).map { bTail =>
              bHead :: bTail
            }
          }
      }

    implicit def productGenericLoader[A[_] <: Product, T1, T2, L1 <: HList, L2 <: HList, I](implicit gen1: Generic.Aux[A[T1], L1], gen2: Generic.Aux[A[T2], L2], loader: ProjectLoader[L1, L2, I]): ProjectLoader[A[T1], A[T2], I] =
      new ProjectLoader[A[T1], A[T2], I] {
        override def loadProject[F[_]](a: A[T1])(implicit monadInstance: Monad[F], fileHandler: ProjectFileHandler[F, I]): F[A[T2]] =
          loader.loadProject(gen1.to(a))(monadInstance, fileHandler).map(gen2.from)
      }

    implicit def mapLoader[K1, K2, V1, V2, I](implicit keyLoader: ProjectLoader[K1, K2, I], valueLoader: ProjectLoader[V1, V2, I]): ProjectLoader[Map[K1, V1], Map[K2, V2], I] =
      new ProjectLoader[Map[K1, V1], Map[K2, V2], I] {
        override def loadProject[F[_]](a: Map[K1, V1])(implicit monadInstance: Monad[F], fileHandler: ProjectFileHandler[F, I]): F[Map[K2, V2]] =
          a
          .toVector
          .traverse { case (k, v) =>
            for {
              k2 <- keyLoader.loadProject(k)(monadInstance, fileHandler)
              v2 <- valueLoader.loadProject(v)(monadInstance, fileHandler)
            } yield k2 -> v2
          }
          .map(_.toMap)
      }

  }



}

trait ProjectFileHandler[F[_], I] {
  def loadSingleFile(file: String): F[I]
}

object ProjectFileHandler {

  def fileHandlerPath(dir: File): ProjectFileHandler[ZIO[FileIO, IOException, ?], Path] = new ProjectFileHandler[ZIO[FileIO, IOException, ?], Path] {

    override def loadSingleFile(file: String): ZIO[FileIO, IOException, Path] =
      IO.effect { dir.toPath.resolve(file) }
        .refineOrDie { case ex: IOException => ex }

  }

}
