package com.mi3software.argon.build.project

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._

import scalaz._
import Scalaz._
import scalaz.zio.IO
import scalaz.zio.interop.scalaz72._
import shapeless.{ Path => _, _ }

trait ProjectLoader[A, B, I] {
  def loadProject[F[_, _]](a: A)(implicit monadInstance: Monad[F[Throwable, ?]], fileHandler: ProjectFileHandler[F, I]): F[Throwable, B]
}

object ProjectLoader {

  def apply[A, B, I](implicit loader: ProjectLoader[A, B, I]): ProjectLoader[A, B, I] = loader

  object Implicits {

    implicit def identityLoader[A, I]: ProjectLoader[A, A, I] = new ProjectLoader[A, A, I] {
      override def loadProject[F[_, _]](a: A)(implicit monadInstance: Monad[F[Throwable, ?]], fileHandler: ProjectFileHandler[F, I]): F[Throwable, A] =
        monadInstance.point(a)
    }

    implicit def fileLoader[I]: ProjectLoader[String, I, I] = new ProjectLoader[String, I, I] {
      override def loadProject[F[_, _]](a: String)(implicit monadInstance: Monad[F[Throwable, ?]], fileHandler: ProjectFileHandler[F, I]): F[Throwable, I] =
        fileHandler.loadSingleFile(a)
    }

    implicit def fileListLoader[I]: ProjectLoader[List[String], List[I], I] = new ProjectLoader[List[String], List[I], I] {
      override def loadProject[F[_, _]](a: List[String])(implicit monadInstance: Monad[F[Throwable, ?]], fileHandler: ProjectFileHandler[F, I]): F[Throwable, List[I]] =
        a.traverseM(fileHandler.loadFileGlob)
    }

    implicit def hconsLoader[AHead, ATail <: HList, BHead, BTail <: HList, I](implicit headLoader: ProjectLoader[AHead, BHead, I], tailLoader: ProjectLoader[ATail, BTail, I]): ProjectLoader[AHead :: ATail, BHead :: BTail, I] =
      new ProjectLoader[AHead :: ATail, BHead :: BTail, I] {
        override def loadProject[F[_, _]](a: AHead :: ATail)(implicit monadInstance: Monad[F[Throwable, ?]], fileHandler: ProjectFileHandler[F, I]): F[Throwable, BHead :: BTail] =
          headLoader.loadProject(a.head)(monadInstance, fileHandler).flatMap { bHead =>
            tailLoader.loadProject(a.tail)(monadInstance, fileHandler).map { bTail =>
              bHead :: bTail
            }
          }
      }

    implicit def productGenericLoader[A[_] <: Product, T1, T2, L1 <: HList, L2 <: HList, I](implicit gen1: Generic.Aux[A[T1], L1], gen2: Generic.Aux[A[T2], L2], loader: ProjectLoader[L1, L2, I]): ProjectLoader[A[T1], A[T2], I] =
      new ProjectLoader[A[T1], A[T2], I] {
        override def loadProject[F[_, _]](a: A[T1])(implicit monadInstance: Monad[F[Throwable, ?]], fileHandler: ProjectFileHandler[F, I]): F[Throwable, A[T2]] =
          loader.loadProject(gen1.to(a))(monadInstance, fileHandler).map(gen2.from)
      }

  }



}

trait ProjectFileHandler[F[_, _], I] {
  def loadSingleFile(file: String): F[Throwable, I]
  def loadFileGlob(glob: String): F[Throwable, List[I]]
}

object ProjectFileHandler {

  def fileHandlerFile(dir: File): ProjectFileHandler[IO, File] = new ProjectFileHandler[IO, File] {

    override def loadSingleFile(file: String): IO[Throwable, File] =
      IO.syncThrowable { new File(dir, file) }

    override def loadFileGlob(glob: String): IO[Throwable, List[File]] = IO.syncThrowable {
      if(glob.contains("*")) {
        val pathMatcher = FileSystems.getDefault.getPathMatcher("glob:" + glob)

        allDirectoryFiles
          .filter { path: Path =>
            pathMatcher.matches(dir.toPath.relativize(path))
          }
          .map { _.toFile }
          .toList
      }
      else {
        List(new File(dir, glob))
      }
    }

    def allDirectoryFiles: Traversable[Path] = new Traversable[Path] {
      override def foreach[U](f: Path => U): Unit = {
        val _ =
          Files.walkFileTree(dir.toPath, new SimpleFileVisitor[Path] {
            override def visitFile(t: Path, basicFileAttributes: BasicFileAttributes): FileVisitResult = {
              val _ = f(t)
              FileVisitResult.CONTINUE
            }
          })
      }
    }

  }

}
