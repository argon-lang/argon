package dev.argon.options

import dev.argon.io.*
import dev.argon.options
import zio.*

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

trait OutputHandler[+E, -Output]:
  def outputs: UIO[Map[Seq[String], OutputInfo[E, Output]]]
end OutputHandler

object OutputHandler:

  inline def derive[E, A <: Product](using m: Mirror.ProductOf[A]): OutputHandler[E, A] =
    lazy val opts = getFieldInfo[E, m.MirroredElemLabels, m.MirroredElemTypes]
    ProductOutputHandler(opts)
  end derive

  inline def getFieldInfo[E, Labels <: Tuple, Elems <: Tuple]: UIO[Map[Seq[String], OutputInfo[E, Elems]]] =
    inline erasedValue[(Labels, Elems)] match {
      case _: (hlabel *: tlabels, helem *: telems) =>
        val label = constValue[hlabel & String]
        val fieldHandler = summonInline[OutputHandler[E, helem]]

        for
          tailFieldInfo <- getFieldInfo[E, tlabels, telems]

          fieldOptions <- fieldHandler.outputs
          fieldInfo = fieldOptions.map { (key, info) =>
            (label +: key) -> HeadFieldOutputInfo[E, helem, telems](info)
          }

          tailFieldInfo2 = tailFieldInfo.view.mapValues { info =>
            TailFieldOutputInfo[E, helem, telems](info)
          }

          fields = fieldInfo ++ tailFieldInfo2

        yield summonInline[Map[Seq[String], OutputInfo[E, helem *: telems]] <:< Map[Seq[String], OutputInfo[E, Elems]]](fields)

      case _: (EmptyTuple, EmptyTuple) =>
        ZIO.succeed(Map.empty)
    }

  final class ProductOutputHandler[E, A <: Product](using m: Mirror.ProductOf[A])(opts: UIO[Map[Seq[String], OutputInfo[E, m.MirroredElemTypes]]]) extends OutputHandler[E, A] {
    override lazy val outputs: UIO[Map[Seq[String], OutputInfo[E, A]]] =
      opts.map(_.view.mapValues { info =>
        new OutputInfo[E, A] {
          override def getValue(options: A): UIO[FileSystemResource[E, BinaryResource]] =
            info.getValue(Tuple.fromProductTyped(options))
        }
      }.toMap)
  }
  

  final class ProductOutputInfo[E, A <: Product](using m: Mirror.ProductOf[A])(info: OutputInfo[E, m.MirroredElemTypes]) extends OutputInfo[E, A] {
    override def getValue(options: A): UIO[FileSystemResource[E, BinaryResource]] =
      info.getValue(Tuple.fromProductTyped(options))
  }

  final class HeadFieldOutputInfo[E, H, T <: Tuple](info: OutputInfo[E, H]) extends OutputInfo[E, H *: T] {
    override def getValue(options: H *: T): UIO[FileSystemResource[E, BinaryResource]] =
      val (h *: _) = options
      info.getValue(h)
    end getValue
  }

  final class TailFieldOutputInfo[E, H, T <: Tuple](info: OutputInfo[E, T]) extends OutputInfo[E, H *: T] {
    override def getValue(options: H *: T): UIO[FileSystemResource[E, BinaryResource]] =
      val (_ *: t) = options
      info.getValue(t)
    end getValue
  }



  given binaryResourceOutputHandler[E, Res <: BinaryResource[E]] => OutputHandler[E, Res]:
    override def outputs: UIO[Map[Seq[String], OutputInfo[E, Res]]] =
      ZIO.succeed(Map(Seq() -> new OutputInfo[E, Res] {
        override def getValue(options: Res): UIO[FileSystemResource[E, BinaryResource]] =
          ZIO.succeed(FileSystemResource.Of(options))
      }))
  end binaryResourceOutputHandler

  given directoryResourceOutputHandler[E, FileRes[+E2] <: BinaryResource[E2], Res <: DirectoryResource[E, FileRes]] => OutputHandler[E, Res]:
    override def outputs: UIO[Map[Seq[String], OutputInfo[E, Res]]] =
      ZIO.succeed(Map(Seq() -> new OutputInfo[E, Res] {
        override def getValue(options: Res): UIO[FileSystemResource[E, BinaryResource]] =
          ZIO.succeed(options)
      }))
  end directoryResourceOutputHandler

  given OutputHandler[Nothing, Nothing]:
    override def outputs: UIO[Map[Seq[String], OutputInfo[Nothing, Nothing]]] = ZIO.succeed(Map.empty)
  end given

end OutputHandler


