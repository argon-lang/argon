package dev.argon.backend

import java.io.IOException

import cats._
import cats.implicits._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import shapeless.{Path => _, _}
import zio._
import zio.interop.catz._

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

    implicit def fileLoader[IOld, I]: ProjectLoader[IOld, I, IOld, I] = new ProjectLoader[IOld, I, IOld, I] {
      override def loadProject[R, E](a: IOld)(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, I] =
        fileHandler.loadSingleFile(a)
    }

    implicit def fileListLoader[IOld, I]: ProjectLoader[List[IOld], List[I], IOld, I] = new ProjectLoader[List[IOld], List[I], IOld, I] {
      override def loadProject[R, E](a: List[IOld])(implicit fileHandler: ProjectFileHandler[R, E, IOld, I]): ZIO[R, E, List[I]] =
        ZIO.foreach(a)(fileHandler.loadSingleFile)
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

trait ProjectFileHandler[-R, +E, -IOld, +I] {
  def loadSingleFile(file: IOld): ZIO[R, E, I]
}

object ProjectFileHandler {

  def fileHandlerPath(dir: Path): ProjectFileHandler[FileIO, IOException, String, PathResourceIndicator] = new ProjectFileHandler[FileIO, IOException, String, PathResourceIndicator] {

    override def loadSingleFile(file: String): ZIO[FileIO, IOException, PathResourceIndicator] =
      Path.of(file).map(dir.resolve).map(PathResourceIndicator.apply)

  }

  def nothingFileHandler[R, E]: ProjectFileHandler[R, E, Nothing, Nothing] = new ProjectFileHandler[R, E, Nothing, Nothing] {
    override def loadSingleFile(file: Nothing): ZIO[R, E, Nothing] = file
  }

}
